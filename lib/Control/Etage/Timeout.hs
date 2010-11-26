{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, NamedFieldPuns #-}

module Control.Etage.Timeout (
  TimeoutNeuron,
  TimeoutFromImpulse,
  TimeoutForImpulse,
  TimeoutOptions,
  NeuronFromImpulse,
  NeuronForImpulse,
  NeuronOptions(..)
) where

import Control.Concurrent
import Control.Monad
import Data.Typeable

import Control.Etage

defaultTimeout :: Int
defaultTimeout = 10000000 -- microseconds, 10 seconds

data TimeoutNeuron = TimeoutNeuron TimeoutOptions deriving (Typeable)

instance Show TimeoutNeuron where
  show = show . typeOf

type TimeoutFromImpulse = NeuronFromImpulse TimeoutNeuron
type TimeoutForImpulse = NeuronForImpulse TimeoutNeuron
type TimeoutOptions = NeuronOptions TimeoutNeuron

instance Impulse TimeoutFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

instance Impulse TimeoutForImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

deriving instance Show TimeoutFromImpulse
deriving instance Show TimeoutForImpulse

instance Neuron TimeoutNeuron where
  data NeuronFromImpulse TimeoutNeuron
  data NeuronForImpulse TimeoutNeuron
  data NeuronOptions TimeoutNeuron = TimeoutOptions {
      timeout :: Int -- microseconds
    } deriving (Eq, Ord, Read, Show)
  
  mkDefaultOptions = return TimeoutOptions {
      timeout = defaultTimeout
    }
  
  grow options = return $ TimeoutNeuron options
  
  live _ n@(TimeoutNeuron TimeoutOptions { timeout }) = do
    threadDelay timeout
    dissolving n
