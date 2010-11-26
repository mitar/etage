{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Externals (
  Neuron(..),
  Impulse(..),
  Nerve,
  AxonConductive,
  AxonNonConductive,
  LiveNeuron,
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
  getNewestForNeuron,
  NeuronMapCapability(..),
  mkNeuronMapOnRandomCapability,
  DissolvingException,
  dissolving,
  DissolveException,
  attach',
  detach,
  detachAndWait,
  detachMany,
  detachManyAndWait,
  ImpulseTranslator(..),
  ImpulseTime,
  ImpulseValue,
  defaultOptions,
  prepareEnvironment,
  translateAndSend,
  Translatable(..),
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
import System.IO
import System.Posix.Signals
import System.Random

import Control.Etage.Chan
import Control.Etage.Internals

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
getForNeuron (Nerve _ NoAxon) = waitForException

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

divideNeuron :: Neuron n => NeuronOptions n -> IO () -> IO NeuronId
divideNeuron options a = fork a
  where fork = case getNeuronMapCapability options of
                 NeuronFreelyMapOnCapability -> forkIO
                 NeuronMapOnCapability c     -> forkOnIO c

deriving instance Typeable1 (NeuronFromImpulse)
deriving instance Typeable1 (NeuronForImpulse)
deriving instance Typeable1 (NeuronOptions)

class (Typeable n, Impulse (NeuronFromImpulse n), Impulse (NeuronForImpulse n)) => Neuron n where
  data NeuronFromImpulse n
  data NeuronForImpulse n
  data NeuronOptions n
  
  mkDefaultOptions :: IO (NeuronOptions n)
  
  getNeuronMapCapability :: NeuronOptions n -> NeuronMapCapability

  grow :: NeuronOptions n -> IO n
  dissolve :: n -> IO ()
  live :: Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> n -> IO ()

  attach :: (NeuronOptions n -> NeuronOptions n) -> Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> IO LiveNeuron

  mkDefaultOptions = return undefined

  getNeuronMapCapability _ = NeuronFreelyMapOnCapability

  grow _ = return undefined
  dissolve _ = return ()
  live _ _ = waitForException
  
  attach = attach'

attach' :: Neuron n => (NeuronOptions n -> NeuronOptions n) -> Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> IO LiveNeuron
attach' optionsSetter nerve = mask $ \restore -> do
  currentThread <- myThreadId
  dissolved <- newEmptyMVar
  defOptions <- mkDefaultOptions
  let options = optionsSetter defOptions
  nid <- divideNeuron options $ do
           bracket (grow options) dissolve (restore . live nerve) `catches` [ -- TODO: Should be dissolve wrapped in uninterruptibleMask
               Handler (\(_ :: DissolveException) -> return ()), -- we ignore DissolveException
               Handler (\(e :: SomeException) -> uninterruptible $ throwTo currentThread e)
             ] `finally` (uninterruptible $ putMVar dissolved ())
  return $ LiveNeuron dissolved nid

data DissolvingException = DissolvingException String deriving (Show, Typeable)

instance Exception DissolvingException

dissolving :: Show n => n -> IO a
dissolving n = throwIO $ DissolvingException (show n)

data DissolveException = DissolveException deriving (Show, Typeable)

instance Exception DissolveException

detach :: LiveNeuron -> IO ()
detach (LiveNeuron _ neuronId) = mask_ . uninterruptible $ throwTo neuronId DissolveException

detachAndWait :: LiveNeuron -> IO ()
detachAndWait n = detachManyAndWait [n]

detachMany :: [LiveNeuron] -> IO ()
detachMany = mask_ . mapM_ detach

detachManyAndWait :: [LiveNeuron] -> IO ()
detachManyAndWait neurons = mask_ $ do
  detachMany neurons
  mapM_ (\(LiveNeuron d _) -> uninterruptible $ takeMVar d) neurons

-- Some operations are interruptible, better than to make them uninterruptible (which can cause deadlocks) we simply retry interrupted operation
-- For this to really work all interruptible operations should be wrapped like this (so it is not good idea to use IO operations in such code sections)
uninterruptible :: IO a -> IO a
uninterruptible a = mask_ $ a `catch` (\(_ :: SomeException) -> uninterruptible a)

class (Impulse i, Impulse j) => ImpulseTranslator i j where
  translate :: i -> [j]

defaultOptions :: Neuron n => NeuronOptions n -> NeuronOptions n
defaultOptions = id

prepareEnvironment :: IO ()
prepareEnvironment = do
  hSetBuffering stderr LineBuffering
  
  mainThreadId <- myThreadId
  
  -- TODO: User interrupt sometimes hangs dissolving (does it still in GHC 7.0?)
  _ <- installHandler keyboardSignal (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigINT
  _ <- installHandler softwareTermination (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigTERM
  
  return ()

translateAndSend :: ImpulseTranslator i for => Nerve from fromConductivity for AxonConductive -> i -> IO ()
translateAndSend nerve i = mapM_ (sendForNeuron nerve) $ translate i

data Translatable i where
  Translatable :: ImpulseTranslator i for => Nerve from fromConductivity for AxonConductive -> Translatable i

getCurrentImpulseTime :: IO ImpulseTime
getCurrentImpulseTime = getPOSIXTime

impulseEq :: (Impulse i, Impulse j) => i -> j -> Bool
impulseEq a b = impulseTime a == impulseTime b && impulseValue a == impulseValue b

impulseCompare :: (Impulse i, Impulse j) => i -> j -> Ordering
impulseCompare a b = (impulseTime a, impulseValue a) `compare` (impulseTime b, impulseValue b)
