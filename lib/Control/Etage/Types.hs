{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Types (
  Neuron(..),
  Impulse(..),
  AxonConductive,
  AxonNonConductive,
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
  DissolvingException(..),
  dissolving,
  DissolveException(..),
  dissolveNeuron,
  ImpulseTranslator(..),
  ImpulseTime,
  ImpulseValue,
  defaultOptions,
  prepareEnvironment,
  translateAndSend,
  Translatable(..),
  Living(..),
  waitForDissolve,
  getCurrentImpulseTime,
  impulseEq,
  impulseCompare
) where

import Prelude hiding (catch)

import Control.Concurrent hiding (Chan, writeChan, readChan, isEmptyChan)
import Data.Data
import Data.Function
import Data.List
import Control.Exception
import Data.Time.Clock.POSIX
import GHC.Conc (forkOnIO, numCapabilities)
import Numeric
import System.IO
import System.Posix.Signals
import System.Random
import Text.ParserCombinators.ReadP

import Control.Etage.Chan

-- TODO: Find better general representation for values (something analog to what a hologram is, so that it can be gradually simplified and gradually reconstructed)
type ImpulseValue = [Rational]

{-|
Type class with common operations for impulses send over 'Nerve's and processed in 'Neuron's.
-}
class (Show i, Typeable i) => Impulse i where
  impulseTime :: i -> ImpulseTime
  impulseValue :: i -> ImpulseValue

data AxonConductive deriving Typeable
data AxonNonConductive deriving Typeable

data Axon impulse conductivity where
  Axon :: Impulse i => Chan i -> Axon i AxonConductive
  NoAxon :: Axon i AxonNonConductive

data Nerve from fromConductivity for forConductivity where
  Nerve :: (Impulse from, Impulse for) => Axon from fromConductivity -> Axon for forConductivity -> Nerve from fromConductivity for forConductivity

sendFromNeuron :: Nerve from fromConductivity for forConductivity -> from -> IO ()
sendFromNeuron (Nerve (Axon chan) _) i = writeChan chan i
sendFromNeuron (Nerve NoAxon _) _ = return () -- we allow sending but ignore so that same Neuron defintion can be used on all kinds of Nerves

getFromNeuron :: Nerve from AxonConductive for forConductivity -> IO from
getFromNeuron (Nerve (Axon chan) _) = readChan chan

maybeGetFromNeuron :: Nerve from AxonConductive for forConductivity -> IO (Maybe from)
maybeGetFromNeuron (Nerve (Axon chan) _) = maybeReadChan chan

slurpFromNeuron :: Nerve from AxonConductive for forConductivity -> IO [from]
slurpFromNeuron (Nerve (Axon chan) _) = slurpChan chan

waitAndSlurpFromNeuron :: Nerve from AxonConductive for forConductivity -> IO [from]
waitAndSlurpFromNeuron nerve = do
  oldest <- getFromNeuron nerve
  others <- slurpFromNeuron nerve
  return $ others ++ [oldest]

sendForNeuron :: Nerve from fromConductivity for AxonConductive -> for -> IO ()
sendForNeuron (Nerve _ (Axon chan)) i = writeChan chan i

getForNeuron :: Nerve from fromConductivity for forConductivity -> IO for
getForNeuron (Nerve _ (Axon chan)) = readChan chan
getForNeuron (Nerve _ NoAxon) = waitForDissolve [] >> return undefined

maybeGetForNeuron :: Nerve from fromConductivity for forConductivity -> IO (Maybe for)
maybeGetForNeuron (Nerve _ (Axon chan)) = maybeReadChan chan
maybeGetForNeuron (Nerve _ NoAxon) = return Nothing -- we allow getting but return Nothing so that same Neuron defintion can be used on all kinds of Nerves

slurpForNeuron :: Nerve from fromConductivity for forConductivity -> IO [for]
slurpForNeuron (Nerve _ (Axon chan)) = slurpChan chan
slurpForNeuron (Nerve _ NoAxon) = return [] -- we allow getting but return [] so that same Neuron defintion can be used on all kinds of Nerves

waitAndSlurpForNeuron :: Nerve from fromConductivity for forConductivity -> IO [for]
waitAndSlurpForNeuron nerve = do
  oldest <- getForNeuron nerve
  others <- slurpForNeuron nerve
  return $ others ++ [oldest]

getNewestForNeuron :: (Data for, Impulse for) => Nerve from fromConductivity for forConductivity -> IO [for]
getNewestForNeuron nerve = do
  impulses <- waitAndSlurpForNeuron nerve
  return $ nubBy ((==) `on` toConstr) $ impulses

maybeReadChan :: Chan a -> IO (Maybe a)
maybeReadChan chan = do
  e <- isEmptyChan chan
  if e
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

data NeuronMapCapability = NeuronMapOnCapability Int | NeuronFreelyMapOnCapability deriving (Eq, Ord, Read, Show)

mkNeuronMapOnRandomCapability :: IO NeuronMapCapability
mkNeuronMapOnRandomCapability = do
  c <- randomRIO (1, numCapabilities)
  return $ NeuronMapOnCapability c

type NeuronDissolved = MVar ()
type NeuronId = ThreadId

-- TODO: Move dissolved MVar handling into divideNeuron
divideNeuron :: Neuron n => NeuronOptions n -> IO () -> IO NeuronId
divideNeuron options a = fork a
  where fork = case getNeuronMapCapability options of
                 NeuronFreelyMapOnCapability -> forkIO
                 NeuronMapOnCapability c     -> forkOnIO c

deriving instance Typeable1 (NeuronFromImpulse)
deriving instance Typeable1 (NeuronForImpulse)
deriving instance Typeable1 (LiveNeuron)
deriving instance Typeable1 (NeuronOptions)

class (Typeable n, Impulse (NeuronFromImpulse n), Impulse (NeuronForImpulse n)) => Neuron n where
  data LiveNeuron n
  data NeuronFromImpulse n
  data NeuronForImpulse n
  data NeuronOptions n

  -- TODO: Once defaults for associated type synonyms are implemented change to that, if possible
  mkLiveNeuron :: NeuronDissolved -> NeuronId -> LiveNeuron n
  getNeuronDissolved :: LiveNeuron n -> NeuronDissolved
  getNeuronId :: LiveNeuron n -> NeuronId
  
  mkDefaultOptions :: IO (NeuronOptions n)
  
  getNeuronMapCapability :: NeuronOptions n -> NeuronMapCapability

  grow :: NeuronOptions n -> IO n
  dissolve :: n -> IO ()
  live :: Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> n -> IO ()

  attach :: (NeuronOptions n -> NeuronOptions n) -> Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> IO (LiveNeuron n)
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

defaultOptions :: Neuron n => NeuronOptions n -> NeuronOptions n
defaultOptions = id

prepareEnvironment :: IO ()
prepareEnvironment = do
  hSetBuffering stderr LineBuffering
  
  mainThreadId <- myThreadId
  
  -- TODO: User interrupt sometimes hangs dissolving
  _ <- installHandler keyboardSignal (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigINT
  _ <- installHandler softwareTermination (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigTERM
  
  return ()

translateAndSend :: ImpulseTranslator i for => Nerve from fromConductivity for AxonConductive -> i -> IO ()
translateAndSend nerve i = mapM_ (sendForNeuron nerve) $ translate i

data Translatable i where
  Translatable :: ImpulseTranslator i for => Nerve from fromConductivity for AxonConductive -> Translatable i

data Living where
  Living :: Neuron n => LiveNeuron n -> Living

-- Blocks thread until an exception arrives and cleans-up afterwards, waiting for all threads to finish
-- Should have MVar computations wrapped in uninterruptible and should not use any IO (because all this can be interrupted despite block)
waitForDissolve :: [Living] -> IO ()
waitForDissolve neurons = block $ do -- TODO: Change block to nonInterruptibleMask? Or remove?
  _ <- (newEmptyMVar >>= takeMVar) `finally` do
    -- TODO: Should also takeMVar go into detach? Or is better to first send exceptions and then wait?
    mapM_ (\(Living l) -> uninterruptible $ detach l) neurons
    mapM_ (\(Living l) -> uninterruptible $ takeMVar . getNeuronDissolved $ l) neurons
  return ()

-- TODO: Remove with GHC 7.0 and masks?
-- Big hack to prevent interruption: it simply retries interrupted computation
uninterruptible :: IO a -> IO a
uninterruptible a = block $ a `catch` (\(_ :: SomeException) -> uninterruptible a)

getCurrentImpulseTime :: IO ImpulseTime
getCurrentImpulseTime = getPOSIXTime

impulseEq :: (Impulse i, Impulse j) => i -> j -> Bool
impulseEq a b = impulseTime a == impulseTime b && impulseValue a == impulseValue b

impulseCompare :: (Impulse i, Impulse j) => i -> j -> Ordering
impulseCompare a b = (impulseTime a, impulseValue a) `compare` (impulseTime b, impulseValue b)
