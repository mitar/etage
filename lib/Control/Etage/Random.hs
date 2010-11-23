{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Random where

import Control.Concurrent
import Control.Monad
import System.Random

import Control.Etage.Types

defaultMaxInterval :: Int
defaultMaxInterval = 1000000 -- microseconds

data (Real r, Show r, Random r) => RandomNeuron r = RandomNeuron (RandomOptions r)

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance (Real r, Show r, Random r) => Impulse (RandomForImpulse r) where
  impulseTime _ = undefined
  impulseValue _ = undefined

instance (Real r, Show r, Random r) => Impulse (RandomFromImpulse r) where
  impulseTime Random { .. } = impulseTimestamp
  impulseValue Random { .. } = [toRational value]

type LiveRandomNeuron r = LiveNeuron (RandomNeuron r)
type RandomForImpulse r = NeuronForImpulse (RandomNeuron r)
type RandomFromImpulse r = NeuronFromImpulse (RandomNeuron r)
type RandomOptions r = NeuronOptions (RandomNeuron r)

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance Show (RandomForImpulse r) where
  show _ = undefined

instance (Real r, Show r, Random r) => Neuron (RandomNeuron r) where
  data LiveNeuron (RandomNeuron r) = LiveRandomNeuron NeuronDissolved NeuronId
  data NeuronForImpulse (RandomNeuron r)
  data NeuronFromImpulse (RandomNeuron r) = Random {
      impulseTimestamp :: ImpulseTime, -- time is first so that ordering is first by time
      value :: r
    } deriving (Eq, Ord, Read, Show)
  data NeuronOptions (RandomNeuron r) = RandomOptions {
      valueSource :: [r],
      intervalSource :: [Int] -- microseconds
    } deriving (Eq, Ord, Read, Show)
  
  mkLiveNeuron = LiveRandomNeuron
  getNeuronDissolved (LiveRandomNeuron dissolved _) = dissolved
  getNeuronId (LiveRandomNeuron _ nid) = nid
  
  mkDefaultOptions = do
    generator <- newStdGen
    generator' <- newStdGen
    return RandomOptions {
        valueSource = randoms generator,
        intervalSource = randomRs (0, defaultMaxInterval) generator'
      }
  
  grow options = return $ RandomNeuron options
  
  live nerve (RandomNeuron RandomOptions { .. }) = forM_ (zip valueSource intervalSource) $ \(v, i) -> do
    time <- getCurrentImpulseTime
    sendFromNeuron nerve $ Random time v
    threadDelay i
