{-# LANGUAGE TypeFamilies, GADTs, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls #-}

module Types where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Typeable
import Numeric
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

getForNeuron :: Nerve a a' b (Chan i) i' d -> IO (Maybe i')
getForNeuron (Nerve _ (Axon chan)) = liftM Just $ readChan chan
getForNeuron (Nerve _ (AxonAny chan)) = liftM Just $ readChan chan
getForNeuron (Nerve _ NoAxon) = return Nothing -- we allow getting but return Nothing so that same Neuron defintion can be used on all kinds of Nerves

maybeGetForNeuron :: Nerve a a' b (Chan i) i' d -> IO (Maybe i')
maybeGetForNeuron (Nerve _ (Axon chan)) = maybeReadChan chan
maybeGetForNeuron (Nerve _ (AxonAny chan)) = maybeReadChan chan
maybeGetForNeuron (Nerve _ NoAxon) = return Nothing -- we allow getting but return Nothing so that same Neuron defintion can be used on all kinds of Nerves

slurpForNeuron :: Nerve a a' b (Chan i) i' d -> IO [i']
slurpForNeuron (Nerve _ (Axon chan)) = slurpChan chan
slurpForNeuron (Nerve _ (AxonAny chan)) = slurpChan chan
slurpForNeuron (Nerve _ NoAxon) = return [] -- we allow getting but return [] so that same Neuron defintion can be used on all kinds of Nerves

maybeReadChan :: Chan a -> IO (Maybe a)
maybeReadChan chan = do
  empty <- isEmptyChan chan
  if empty
    then return Nothing
    else do
      c <- readChan chan
      return $ Just c

-- First-in element in the channel is last in the list
slurpChan :: Chan a -> IO [a]
slurpChan chan = slurpChan' []
  where slurpChan' cs = do
          mc <- maybeReadChan chan
          case mc of
            Nothing -> return cs
            Just c  -> slurpChan' (c:cs)

type NeuronId = ThreadId

class Neuron n where
  data LiveNeuron n
  data NeuronImpulse n -- it should be made an instance of Impulse

  -- TODO: Once defaults for associated type synonyms are implemented change to that, if possible
  mkLiveNeuron :: NeuronId -> LiveNeuron n
  getNeuronId :: LiveNeuron n -> NeuronId

  grow :: IO n
  dissolve :: n -> IO ()
  live :: Show a' => Nerve (Chan (NeuronImpulse n)) a' b (Chan (NeuronImpulse n)) (NeuronImpulse n) d -> n -> IO ()

  attach :: Show a' => Nerve (Chan (NeuronImpulse n)) a' b (Chan (NeuronImpulse n)) (NeuronImpulse n) d -> IO (LiveNeuron n)
  deattach :: LiveNeuron n -> IO ()

  grow = return undefined
  dissolve _ = return ()
  attach nerve = do
    currentThread <- myThreadId
    ((liftM mkLiveNeuron) . forkIO $ handle (throwTo currentThread :: SomeException -> IO ()) $
      bracket (grow :: IO n) dissolve (live nerve)) :: IO (LiveNeuron n)
  deattach = killThread . getNeuronId

data (Show a, Typeable a) => DissolvingException a = DissolvingException a deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (DissolvingException a)

dissolving :: (Show s, Typeable s) => s -> IO a
dissolving s = throwIO $ DissolvingException s

type ImpulseTime = POSIXTime

instance Read ImpulseTime where
  readsPrec _ r = do
    (time, sec) <- readFloat r
    ('s', rest) <- readP_to_S (char 's') sec
    return (time, rest)

-- blocks thread until an exception arrives
waitForException :: IO ()
waitForException = newEmptyMVar >>= takeMVar
