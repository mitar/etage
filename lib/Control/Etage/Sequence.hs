{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Sequence (
  SequenceNeuron,
  SequenceFromImpulse,
  SequenceForImpulse,
  SequenceOptions,
  NeuronFromImpulse(..),
  NeuronForImpulse,
  NeuronOptions(..)
) where

import Control.Concurrent
import Control.Monad
import Data.Typeable
import System.Random

import Control.Etage

defaultMaxInterval :: Int
defaultMaxInterval = 1000000 -- microseconds

data (Real r, Random r, Show r, Typeable r) => SequenceNeuron r = SequenceNeuron (SequenceOptions r) deriving (Typeable)

type SequenceFromImpulse r = NeuronFromImpulse (SequenceNeuron r)
type SequenceForImpulse r = NeuronForImpulse (SequenceNeuron r)
type SequenceOptions r = NeuronOptions (SequenceNeuron r)

instance (Real r, Random r, Show r, Typeable r) => Impulse (SequenceFromImpulse r) where
  impulseTime Value { .. } = impulseTimestamp
  impulseValue Value { .. } = [toRational value]

instance (Real r, Random r, Show r, Typeable r) => Impulse (SequenceForImpulse r) where
  impulseTime _ = undefined
  impulseValue _ = undefined

deriving instance Show (SequenceForImpulse r)

instance (Real r, Random r, Show r, Typeable r) => Neuron (SequenceNeuron r) where
  data LiveNeuron (SequenceNeuron r) = LiveSequenceNeuron NeuronDissolved NeuronId
  data NeuronFromImpulse (SequenceNeuron r) = Value {
      impulseTimestamp :: ImpulseTime, -- time is first so that ordering is first by time
      value :: r
    } deriving (Eq, Ord, Read, Show)
  data NeuronForImpulse (SequenceNeuron r)
  data NeuronOptions (SequenceNeuron r) = SequenceOptions {
      valueSource :: [r],
      intervalSource :: [Int] -- microseconds
    } deriving (Eq, Ord, Read, Show)
  
  mkLiveNeuron = LiveSequenceNeuron
  getNeuronDissolved (LiveSequenceNeuron dissolved _) = dissolved
  getNeuronId (LiveSequenceNeuron _ nid) = nid
  
  mkDefaultOptions = do
    generator <- newStdGen
    generator' <- newStdGen
    return SequenceOptions {
        valueSource = randoms generator,
        intervalSource = randomRs (0, defaultMaxInterval) generator'
      }
  
  grow options = return $ SequenceNeuron options
  
  live nerve (SequenceNeuron SequenceOptions { .. }) = forM_ (zip valueSource intervalSource) $ \(v, i) -> do
    time <- getCurrentImpulseTime
    sendFromNeuron nerve $ Value time v
    threadDelay i
