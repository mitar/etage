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

data AnyImpulse :: * where
  AnyImpulse :: Impulse i => i -> AnyImpulse
data NoImpulse

deriving instance Show (AnyImpulse)

data AxonConductive
data AxonNotConductive

data Axon :: * -> * -> * -> * where -- Axon, type of channel elements, type of elements possible to get from the channel, is axon conductive
  Axon :: Impulse i => Chan i -> Axon (Chan i) i AxonConductive
  AxonAny :: Chan AnyImpulse -> Axon (Chan i) AnyImpulse AxonConductive
  NoAxon :: Axon (Chan i) NoImpulse AxonNotConductive

data Nerve :: * -> * -> * -> * -> * -> * -> * where
  Nerve :: Axon a a' b -> Axon c c' d -> Nerve a a' b c c' d

sendFromNeuron :: Impulse i => Nerve (Chan i) i' b c c' d -> i -> IO ()
sendFromNeuron (Nerve (Axon chan) _) i = writeChan chan i
sendFromNeuron (Nerve (AxonAny chan) _) i = writeChan chan $ AnyImpulse i
sendFromNeuron (Nerve NoAxon _) _ = return () -- we allow sending but ignore so that same Neuron defintion can be used on all kinds of Nerves

getFromNeuron :: Nerve (Chan i) i' AxonConductive c c' d -> IO (Maybe i')
getFromNeuron (Nerve (Axon chan) _) = maybeReadChan chan
getFromNeuron (Nerve (AxonAny chan) _) = maybeReadChan chan

sendForNeuron :: Impulse i => Nerve a a' b (Chan i) i' AxonConductive -> i -> IO ()
sendForNeuron (Nerve _ (Axon chan)) i = writeChan chan i
sendForNeuron (Nerve _ (AxonAny chan)) i = writeChan chan $ AnyImpulse i

getForNeuron :: Nerve a a' b (Chan i) i' d -> IO (Maybe i')
getForNeuron (Nerve _ (Axon chan)) = maybeReadChan chan
getForNeuron (Nerve _ (AxonAny chan)) = maybeReadChan chan
getForNeuron (Nerve _ NoAxon) = return Nothing -- we allow getting but return Nothing so that same Neuron defintion can be used on all kinds of Nerves

maybeReadChan :: Chan a -> IO (Maybe a)
maybeReadChan chan = do
  empty <- isEmptyChan chan
  if empty
    then return Nothing
    else do
      c <- readChan chan
      return $ Just c

type NeuronId = ThreadId

class Neuron n where
  data LiveNeuron n
  data NeuronImpulse n -- it should be made an instance of Impulse

  -- TODO: Once defaults for associated type synonyms are implemented change to that, if possible
  mkLiveNeuron :: NeuronId -> LiveNeuron n
  getNeuronId :: LiveNeuron n -> NeuronId

  grow :: IO n
  dissolve :: n -> IO ()
  live :: Nerve (Chan (NeuronImpulse n)) a' b (Chan (NeuronImpulse n)) c' d -> n -> IO ()

  attach :: Nerve (Chan (NeuronImpulse n)) a' b (Chan (NeuronImpulse n)) c' d -> IO (LiveNeuron n)
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
