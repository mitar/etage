{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, NamedFieldPuns #-}

{-|
This module defines a simple 'Neuron' which just fails (throws a 'DissolvingException') in 'grow'ing phase. It can be used to test
error recovery and cleanup in 'grow'ing phase or early stages of 'live'ing phase in other 'Neuron's by using something like:

> _ <- growNeuron defaultOptions :: NerveNone FailNeuron

somewhere among (or after) 'growNeuron' calls for other 'Neuron's in 'Incubation'.
-}

module Control.Etage.Fail (
  FailNeuron,
  FailFromImpulse,
  FailForImpulse,
  FailOptions,
  NeuronFromImpulse,
  NeuronForImpulse,
  NeuronOptions(..)
) where

import Control.Concurrent
import Data.Typeable

import Control.Etage

data FailNeuron deriving (Typeable)

instance Show FailNeuron where
  show = show . typeOf

-- | 'Impulse's from 'FailNeuron'. This 'Neuron' does not define any 'Impulse's it would send.
type FailFromImpulse = NeuronFromImpulse FailNeuron
-- | 'Impulse's for 'FailNeuron'. This 'Neuron' does not define any 'Impulse's it would receive.
type FailForImpulse = NeuronForImpulse FailNeuron
{-| Options for 'FailNeuron'. This option is defined:

[@delay :: 'Int'@] The delay in microseconds before 'Neuron' fails. Default is no delay.
-}
type FailOptions = NeuronOptions FailNeuron

-- | Impulse instance for 'FailNeuron'.
instance Impulse FailFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

-- | Impulse instance for 'FailNeuron'.
instance Impulse FailForImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

deriving instance Show FailFromImpulse
deriving instance Show FailForImpulse

-- | A simple 'Neuron' which just fails in 'grow'ing phase.
instance Neuron FailNeuron where
  data NeuronFromImpulse FailNeuron
  data NeuronForImpulse FailNeuron
  data NeuronOptions FailNeuron = FailOptions {
      delay :: Int
    } deriving (Eq, Ord, Read, Show)
  
  mkDefaultOptions = return FailOptions {
      delay = 0
    }
  
  grow FailOptions { delay } = do
    threadDelay delay
    dissolving (undefined :: FailNeuron)
