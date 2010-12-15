{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, NamedFieldPuns #-}

{-|
This module defines a simple 'Neuron' which initiates 'dissolving' after a given delay. It can be used to limit execution time of
the network. You 'grow' it in 'Incubation' by using something like:

> _ <- growNeuron defaultOptions :: NerveNone TimeoutNeuron

somewhere among (best at the end) 'growNeuron' calls for other 'Neuron's in 'Incubation'.
-}

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
import Data.Data

import Control.Etage

defaultTimeout :: Int
defaultTimeout = 60000000 -- microseconds, 60 seconds

data TimeoutNeuron = TimeoutNeuron TimeoutOptions deriving (Typeable, Data)

instance Show TimeoutNeuron where
  show = show . typeOf

-- | 'Impulse's from 'TimeoutNeuron'. This 'Neuron' does not define any 'Impulse's it would send.
type TimeoutFromImpulse = NeuronFromImpulse TimeoutNeuron
-- | 'Impulse's for 'TimeoutNeuron'. This 'Neuron' does not define any 'Impulse's it would receive.
type TimeoutForImpulse = NeuronForImpulse TimeoutNeuron
{-|
Options for 'TimeoutNeuron'. This option is defined:

[@timeout :: 'Int'@] The length of the delay in microseconds before initiating 'dissolving'. Default is 60 seconds.
-}
type TimeoutOptions = NeuronOptions TimeoutNeuron

-- | Impulse instance for 'TimeoutNeuron'.
instance Impulse TimeoutFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

-- | Impulse instance for 'TimeoutNeuron'.
instance Impulse TimeoutForImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

deriving instance Show TimeoutFromImpulse
deriving instance Show TimeoutForImpulse

deriving instance Data TimeoutFromImpulse
deriving instance Data TimeoutForImpulse

-- | A simple 'Neuron' which initiates 'dissolving' after a given delay.
instance Neuron TimeoutNeuron where
  data NeuronFromImpulse TimeoutNeuron
  data NeuronForImpulse TimeoutNeuron
  data NeuronOptions TimeoutNeuron = TimeoutOptions {
      timeout :: Int -- microseconds
    } deriving (Eq, Ord, Read, Show, Data)
  
  mkDefaultOptions = return TimeoutOptions {
      timeout = defaultTimeout
    }
  
  grow options = return $ TimeoutNeuron options
  
  live _ n@(TimeoutNeuron TimeoutOptions { timeout }) = do
    threadDelay timeout
    dissolving n
