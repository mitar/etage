{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage (
  Impulse,
  AnyImpulse(..),
  AxonConductive,
  AxonNotConductive,
  Axon(..),
  Nerve(..),
  sendFromNeuron,
  getFromNeuron,
  maybeGetFromNeuron,
  slurpFromNeuron,
  waitAndSlurpFromNeuron,
  sendForNeuron,
  getForNeuron,
  maybeGetForNeuron,
  slurpForNeuron,
  waitAndSlurpForNeuron,
  NeuronMapCapability(..),
  mkNeuronMapOnRandomCapability,
  NeuronDissolved,
  NeuronId,
  divideNeuron,
  Neuron(..),
  DissolvingException(..),
  dissolving,
  DissolveException(..),
  dissolveNeuron,
  ImpulseTranslator(..),
  ImpulseTime,
  axon,
  axonAny,
  noAxon,
  growNerve,
  defaultOptions,
  growEnvironment,
  translateAndSend,
  Translatable(..),
  propagate,
  Growable(..),
  growNeurons,
  waitForDissolve,
  uninterruptible
) where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Typeable
import GHC.Conc (forkOnIO, numCapabilities)
import Numeric
import System.IO
import System.Posix.Signals
import System.Random
import Text.ParserCombinators.ReadP

-- TODO: Impulse cannot really be shown when taken from Nerve?
-- TODO: Implement delay Neuron (constant delay, random from some distribution)

class Show a => Impulse a where
  
-- TODO: Move to a special Neuron which can accept different kinds of input (default Neurons can accept only their Impulses)
-- TODO: Make example "any" Neuron which dumps everything it passes through
data AnyImpulse :: * where
  AnyImpulse :: Impulse i => i -> AnyImpulse

deriving instance Show (AnyImpulse)

data AxonConductive
data AxonNotConductive

data Axon :: * -> * -> * -> * where -- Axon, type of channel elements, type of elements possible to get from the channel, is axon conductive
  Axon :: Impulse i => Chan i -> Axon (Chan i) i AxonConductive
  AxonAny :: Chan AnyImpulse -> Axon (Chan i) AnyImpulse AxonConductive
  NoAxon :: Axon (Chan i) i AxonNotConductive

data Nerve :: * -> * -> * -> * -> * -> * -> * where
  Nerve :: Axon a a' b -> Axon c c' d -> Nerve a a' b c c' d

sendFromNeuron :: Impulse i => Nerve (Chan i) i' b c c' d -> i -> IO ()
sendFromNeuron (Nerve (Axon chan) _) i = writeChan chan i
sendFromNeuron (Nerve (AxonAny chan) _) i = writeChan chan $ AnyImpulse i
sendFromNeuron (Nerve NoAxon _) _ = return () -- we allow sending but ignore so that same Neuron defintion can be used on all kinds of Nerves

getFromNeuron :: Nerve (Chan i) i' AxonConductive c c' d -> IO i'
getFromNeuron (Nerve (Axon chan) _) = readChan chan
getFromNeuron (Nerve (AxonAny chan) _) = readChan chan

maybeGetFromNeuron :: Nerve (Chan i) i' AxonConductive c c' d -> IO (Maybe i')
maybeGetFromNeuron (Nerve (Axon chan) _) = maybeReadChan chan
maybeGetFromNeuron (Nerve (AxonAny chan) _) = maybeReadChan chan

slurpFromNeuron :: Nerve (Chan i) i' AxonConductive c c' d -> IO [i']
slurpFromNeuron (Nerve (Axon chan) _) = slurpChan chan
slurpFromNeuron (Nerve (AxonAny chan) _) = slurpChan chan

waitAndSlurpFromNeuron :: Nerve (Chan i) i' AxonConductive c c' d -> IO [i']
waitAndSlurpFromNeuron nerve = do
  oldest <- getFromNeuron nerve
  others <- slurpFromNeuron nerve
  return $ others ++ [oldest]

sendForNeuron :: Impulse i => Nerve a a' b (Chan i) i' AxonConductive -> i -> IO ()
sendForNeuron (Nerve _ (Axon chan)) i = writeChan chan i
sendForNeuron (Nerve _ (AxonAny chan)) i = writeChan chan $ AnyImpulse i

getForNeuron :: Nerve a a' b (Chan i) i' d -> IO i'
getForNeuron (Nerve _ (Axon chan)) = readChan chan
getForNeuron (Nerve _ (AxonAny chan)) = readChan chan
getForNeuron (Nerve _ NoAxon) = waitForDissolve [] >> return undefined

maybeGetForNeuron :: Nerve a a' b (Chan i) i' d -> IO (Maybe i')
maybeGetForNeuron (Nerve _ (Axon chan)) = maybeReadChan chan
maybeGetForNeuron (Nerve _ (AxonAny chan)) = maybeReadChan chan
maybeGetForNeuron (Nerve _ NoAxon) = return Nothing -- we allow getting but return Nothing so that same Neuron defintion can be used on all kinds of Nerves

slurpForNeuron :: Nerve a a' b (Chan i) i' d -> IO [i']
slurpForNeuron (Nerve _ (Axon chan)) = slurpChan chan
slurpForNeuron (Nerve _ (AxonAny chan)) = slurpChan chan
slurpForNeuron (Nerve _ NoAxon) = return [] -- we allow getting but return [] so that same Neuron defintion can be used on all kinds of Nerves

waitAndSlurpForNeuron :: Nerve a a' b (Chan i) i' d -> IO [i']
waitAndSlurpForNeuron nerve = do
  oldest <- getForNeuron nerve
  others <- slurpForNeuron nerve
  return $ others ++ [oldest]

{-
-- TODO: Enable in GHC 7.0
getNewestForNeuron :: Data i' => Nerve a a' b (Chan i) i' d -> IO [i']
getNewestForNeuron nerve = do
  impulses <- waitAndSlurpForNeuron nerve
  return $ nubBy ((==) `on` toConstr) $ impulses
-}

maybeReadChan :: Chan a -> IO (Maybe a)
maybeReadChan chan = do
  empty <- isEmptyChan chan
  if empty
    then return Nothing
    else do
      c <- readChan chan
      return $ Just c

-- First-in (oldest) element in the channel is last in the list
slurpChan :: Chan a -> IO [a]
slurpChan chan = slurpChan' []
  where slurpChan' cs = do
          mc <- maybeReadChan chan
          case mc of
            Nothing -> return cs
            Just c  -> slurpChan' (c:cs)

data NeuronMapCapability = NeuronMapOnCapability Int | NeuronFreelyMapOnCapability deriving (Eq, Show)

mkNeuronMapOnRandomCapability :: IO NeuronMapCapability
mkNeuronMapOnRandomCapability = do
  c <- randomRIO (1, numCapabilities)
  return $ NeuronMapOnCapability c

type NeuronDissolved = MVar ()
type NeuronId = ThreadId

-- TODO: Move dissolved MVar handling into divideNeuron
-- TODO: Do not export?
divideNeuron :: Neuron n => NeuronOptions n -> IO () -> IO NeuronId
divideNeuron options a = fork a
  where fork = case getNeuronMapCapability options of
                 NeuronFreelyMapOnCapability -> forkIO
                 NeuronMapOnCapability c     -> forkOnIO c

-- TODO: Add Data in Typeable requirements for NeuronForImpulse and NeuronFromImpulse in GHC 7.0
class (Impulse (NeuronForImpulse n), Impulse (NeuronFromImpulse n)) => Neuron n where
  data LiveNeuron n
  data NeuronForImpulse n
  data NeuronFromImpulse n
  data NeuronOptions n

  -- TODO: Once defaults for associated type synonyms are implemented change to that, if possible
  mkLiveNeuron :: NeuronDissolved -> NeuronId -> LiveNeuron n
  getNeuronDissolved :: LiveNeuron n -> NeuronDissolved
  getNeuronId :: LiveNeuron n -> NeuronId
  
  mkDefaultOptions :: IO (NeuronOptions n)
  
  getNeuronMapCapability :: NeuronOptions n -> NeuronMapCapability

  grow :: NeuronOptions n -> IO n
  dissolve :: n -> IO ()
  live :: Nerve (Chan (NeuronFromImpulse n)) a' b (Chan (NeuronForImpulse n)) (NeuronForImpulse n) d -> n -> IO ()

  attach :: (NeuronOptions n -> NeuronOptions n) -> Nerve (Chan (NeuronFromImpulse n)) a' b (Chan (NeuronForImpulse n)) (NeuronForImpulse n) d -> IO (LiveNeuron n)
  detach :: LiveNeuron n -> IO ()

  mkDefaultOptions = return undefined

  getNeuronMapCapability _ = NeuronFreelyMapOnCapability

  grow _ = return undefined
  dissolve _ = return ()
  live _ _ = waitForDissolve []
  
  -- TODO: Move default implementation out of the class so that it can be reused/wrapped around in some other class instance definition
  attach optionsSetter nerve = do
    currentThread <- myThreadId
    dissolved <- newEmptyMVar
    defOptions <- mkDefaultOptions
    let options = optionsSetter defOptions
        sequel = putMVar dissolved ()
        run = do
          bracket (grow options) dissolve (unblock . live nerve) `catches` [
              Handler (\(_ :: DissolveException) -> return ()), -- we ignore DissolveException
              Handler (throwTo currentThread :: SomeException -> IO ())
            ] `onException` sequel -- TODO: Change to finally in GHC 7.0
          sequel
    nid <- block $ divideNeuron options run -- TODO: Change block to nonInterruptibleMask? Or remove? Or move to divideNeuron?
    return $ mkLiveNeuron dissolved nid
  detach = dissolveNeuron

data DissolvingException = DissolvingException String deriving (Show, Typeable)

instance Exception DissolvingException

dissolving :: Show n => n -> IO a
dissolving n = throwIO $ DissolvingException (show n)

data DissolveException = DissolveException deriving (Show, Typeable)

instance Exception DissolveException

dissolveNeuron :: Neuron n => LiveNeuron n -> IO ()
dissolveNeuron n = throwTo (getNeuronId n) DissolveException

class (Impulse i, Impulse j) => ImpulseTranslator i j where
  translate :: i -> [j]

type ImpulseTime = POSIXTime

instance Read ImpulseTime where
  readsPrec _ r = do
    (time, sec) <- readFloat r
    ('s', rest) <- readP_to_S (char 's') sec
    return (time, rest)

axon :: Impulse i => IO (Axon (Chan i) i AxonConductive)
axon = do
 chan <- newChan
 return (Axon chan)

axonAny :: IO (Axon (Chan i) AnyImpulse AxonConductive)
axonAny = do
 chan <- newChan
 return (AxonAny chan)

noAxon :: IO (Axon (Chan i) i AxonNotConductive)
noAxon = return NoAxon

growNerve :: IO (Axon a a' b) -> IO (Axon c c' d) -> IO (Nerve a a' b c c' d)
growNerve growFrom growFor = do
 from <- growFrom
 for <- growFor
 return $ Nerve from for

defaultOptions :: Neuron n => NeuronOptions n -> NeuronOptions n
defaultOptions = id

growEnvironment :: IO ()
growEnvironment = do
  hSetBuffering stderr LineBuffering
  
  mainThreadId <- myThreadId
  
  -- TODO: User interrupt sometimes hangs dissolving
  _ <- installHandler keyboardSignal (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigINT
  _ <- installHandler softwareTermination (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigTERM
  
  return ()

translateAndSend :: ImpulseTranslator i c => Nerve a a' b (Chan c) c' AxonConductive -> i -> IO ()
translateAndSend nerve i = mapM_ (sendForNeuron nerve) $ translate i

data Translatable i where
  Translatable :: ImpulseTranslator i c => Nerve a a' b (Chan c) c' AxonConductive -> Translatable i

data PropagateNeuron a a' c c' d = PropagateNeuron (PropagateOptions a a' c c' d)

instance Impulse (PropagateForImpulse a a' c c' d)
instance Impulse (PropagateFromImpulse a a' c c' d)

type LivePropagateNeuron a a' c c' d = LiveNeuron (PropagateNeuron a a' c c' d)
type PropagateForImpulse a a' c c' d = NeuronForImpulse (PropagateNeuron a a' c c' d)
type PropagateFromImpulse a a' c c' d = NeuronFromImpulse (PropagateNeuron a a' c c' d)
type PropagateOptions a a' c c' d = NeuronOptions (PropagateNeuron a a' c c' d)

-- TODO: Remove in favor of automatic deriving in GHC 7.0
instance Show (PropagateForImpulse a a' c c' d) where
  show _ = "PropagateForImpulse"

instance Show (PropagateFromImpulse a a' c c' d) where
  show _ = "PropagateFromImpulse"

instance Neuron (PropagateNeuron a a' c c' d) where
  data LiveNeuron (PropagateNeuron a a' c c' d) = LivePropagateNeuron NeuronDissolved NeuronId
  data NeuronForImpulse (PropagateNeuron a a' c c' d)
  data NeuronFromImpulse (PropagateNeuron a a' c c' d)
  data NeuronOptions (PropagateNeuron a a' c c' d) = PropagateOptions {
      from :: Nerve (Chan a) a' AxonConductive c c' d,
      for ::[Translatable a']
    }
  
  mkLiveNeuron = LivePropagateNeuron
  getNeuronDissolved (LivePropagateNeuron dissolved _) = dissolved
  getNeuronId (LivePropagateNeuron _ nid) = nid
  
  mkDefaultOptions = return PropagateOptions { from = undefined, for = undefined }
  
  grow options = return $ PropagateNeuron options
  
  live _ (PropagateNeuron PropagateOptions { .. }) = forever $ do
    i <- getFromNeuron from
    mapM_ (\(Translatable n) -> translateAndSend n i) for

propagate :: forall a a' c c' d. Nerve (Chan a) a' AxonConductive c c' d -> [Translatable a'] -> IO ()
propagate from for = do
  -- we do not manage this neuron, it will be cleaned by RTS at program exit
  _ <- attach (\o -> o { from, for }) undefined :: IO (LivePropagateNeuron a a' c c' d)
  return ()

data Growable where
  Growable :: Neuron n => LiveNeuron n -> Growable

-- TODO: Change this into a monad and use do notation?
growNeurons :: [IO Growable] -> IO [Growable]
growNeurons attaches = growNeurons' attaches []
  where growNeurons' [] ls      = return ls
        growNeurons' (a:ats) ls = bracketOnError a (\(Growable l) -> detach l) (\l -> growNeurons' ats (l:ls))

-- Blocks thread until an exception arrives and cleans-up afterwards, waiting for all threads to finish
-- Should have MVar computations wrapped in uninterruptible and should not use any IO (because all this can be interrupted despite block)
waitForDissolve :: [Growable] -> IO ()
waitForDissolve neurons = block $ do -- TODO: Change block to nonInterruptibleMask? Or remove?
  _ <- (newEmptyMVar >>= takeMVar) `finally` do
    -- TODO: Should also takeMVar go into detach? Or is better to first send exceptions and then wait?
    mapM_ (\(Growable l) -> uninterruptible $ detach l) neurons
    mapM_ (\(Growable l) -> uninterruptible $ takeMVar . getNeuronDissolved $ l) neurons
  return ()

-- TODO: Remove with GHC 7.0 and masks?
-- Big hack to prevent interruption: it simply retries interrupted computation
uninterruptible :: IO a -> IO a
uninterruptible a = block $ a `catch` (\(_ :: SomeException) -> uninterruptible a)
