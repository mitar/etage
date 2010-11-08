{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls #-}

module Types where

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

class Show a => Impulse a

-- TODO: Move to a special Neuron which can accept different kinds of input (default Neurons can accept only their Impulses)
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

getNewestForNeuron :: Nerve a a' b (Chan i) i' d -> IO i'
getNewestForNeuron nerve = do
  oldest <- getForNeuron nerve
  others <- slurpForNeuron nerve
  return $ if null others
    then oldest
    else head others

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

type NeuronId = ThreadId

divideNeuron :: Neuron n => NeuronOptions n -> IO () -> IO NeuronId
divideNeuron options a = fork a
  where fork = case getNeuronMapCapability options of
                 NeuronFreelyMapOnCapability -> forkIO
                 NeuronMapOnCapability c     -> forkOnIO c

class (Impulse (NeuronForImpulse n), Impulse (NeuronFromImpulse n)) => Neuron n where
  data LiveNeuron n
  data NeuronForImpulse n
  data NeuronFromImpulse n
  data NeuronOptions n

  -- TODO: Once defaults for associated type synonyms are implemented change to that, if possible
  mkLiveNeuron :: NeuronId -> LiveNeuron n
  getNeuronId :: LiveNeuron n -> NeuronId
  
  mkDefaultOptions :: IO (NeuronOptions n)
  
  getNeuronMapCapability :: NeuronOptions n -> NeuronMapCapability

  grow :: NeuronOptions n -> IO n
  dissolve :: n -> IO ()
  live :: Show a' => Nerve (Chan (NeuronFromImpulse n)) a' b (Chan (NeuronForImpulse n)) (NeuronForImpulse n) d -> n -> IO ()

  attach :: Show a' => (NeuronOptions n -> NeuronOptions n) -> Nerve (Chan (NeuronFromImpulse n)) a' b (Chan (NeuronForImpulse n)) (NeuronForImpulse n) d -> IO (LiveNeuron n)
  deattach :: LiveNeuron n -> IO ()

  mkDefaultOptions = return undefined

  getNeuronMapCapability _ = NeuronFreelyMapOnCapability

  grow _ = return undefined
  dissolve _ = return ()
  live _ _ = waitForDissolve []
  
  attach optionsSetter nerve = do
    currentThread <- myThreadId
    defOptions <- mkDefaultOptions
    let options = optionsSetter defOptions
    ((liftM mkLiveNeuron) . divideNeuron options $ handle (throwTo currentThread :: SomeException -> IO ()) $
      bracket (grow options :: IO n) (block . dissolve) (live nerve)) :: IO (LiveNeuron n) -- TODO: Change block to nonInterruptibleMask? Or remove?
  deattach = killThread . getNeuronId

data (Show a, Typeable a) => DissolvingException a = DissolvingException a deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (DissolvingException a)

dissolving :: (Show s, Typeable s) => s -> IO a
dissolving s = throwIO $ DissolvingException s

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

-- TODO: Could be probably simplified once defaults for associated type synonyms are implemented
data EmptyNeuron

instance Impulse EmptyForImpulse
instance Impulse EmptyFromImpulse

type LiveEmptyNeuron = LiveNeuron EmptyNeuron
type EmptyForImpulse = NeuronForImpulse EmptyNeuron
type EmptyFromImpulse = NeuronFromImpulse EmptyNeuron
type EmptyOptions = NeuronOptions EmptyNeuron

instance Show EmptyForImpulse where
  show _ = "EmptyForImpulse"

instance Show EmptyFromImpulse where
  show _ = "EmptyFromImpulse"

instance Neuron EmptyNeuron where
  data LiveNeuron EmptyNeuron = LiveEmptyNeuron NeuronId
  data NeuronForImpulse EmptyNeuron
  data NeuronFromImpulse EmptyNeuron
  data NeuronOptions EmptyNeuron = EmptyOptions {}
  
  mkLiveNeuron nid = LiveEmptyNeuron nid
  getNeuronId (LiveEmptyNeuron nid) = nid

growEnvironment :: IO ()
growEnvironment = do
  hSetBuffering stderr LineBuffering
  
  mainThreadId <- myThreadId
  
  _ <- installHandler keyboardSignal (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigINT
  _ <- installHandler softwareTermination (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigTERM
  
  return ()

translateAndSend :: ImpulseTranslator i c => Nerve a a' b (Chan c) c' AxonConductive -> i -> IO ()
translateAndSend nerve i = do
  mapM_ (sendForNeuron nerve) $ translate i

data Translatable i where
  Translatable :: ImpulseTranslator i c => Nerve a a' b (Chan c) c' AxonConductive -> Translatable i

propagate :: Nerve (Chan a) a' AxonConductive c c' d -> [Translatable a'] -> IO ()
propagate from for = do
  options <- mkDefaultOptions :: IO EmptyOptions
  _ <- divideNeuron options $ forever $ do
    i <- getFromNeuron from
    mapM_ (\(Translatable n) -> translateAndSend n i) for
  return ()

data Growable where
  Growable :: Neuron n => LiveNeuron n -> Growable

growNeurons :: [IO Growable] -> IO [Growable]
growNeurons attaches = growNeurons' attaches []
  where growNeurons' [] ls      = return ls
        growNeurons' (a:ats) ls = bracketOnError a (\(Growable l) -> deattach l) (\l -> growNeurons' ats (l:ls))

-- blocks thread until an exception arrives and cleans-up afterwards
waitForDissolve :: [Growable] -> IO ()
waitForDissolve neurons = do
  _ <- newEmptyMVar >>= takeMVar
  block $ mapM_ (\(Growable l) -> deattach l) neurons -- TODO: Change block to nonInterruptibleMask? Or remove?
