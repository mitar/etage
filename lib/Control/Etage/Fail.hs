{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, EmptyDataDecls, NamedFieldPuns #-}

{-|
This module defines a simple 'Neuron' which just fails (throws a 'DissolvingException') in 'grow'ing phase. It can be used to test
error recovery and cleanup in 'grow'ing phase or early stages of 'live'ing phase in other 'Neuron's by using something like:

> _ <- (growNeuron :: NerveNone FailNeuron) (\o -> o { delay = 10000000 })

somewhere among (or after) 'growNeuron' calls for other 'Neuron's in 'Incubation'.

This 'Neuron' does not process any 'Impulse's.
-}

module Control.Etage.Fail (
  FailNeuron,
  FailFromImpulse,
  FailForImpulse,
  FailOptions,
  NeuronOptions(..)
) where

import Control.Concurrent
import Data.Data

import Control.Etage

defaultDelay :: Int
defaultDelay = 0 -- microseconds

data FailNeuron deriving (Typeable)

deriving instance Data FailNeuron

instance Show FailNeuron where
  show = show . typeOf

-- | 'Impulse's from 'FailNeuron'. This 'Neuron' does not define any 'Impulse's it would send, 'NoImpulse'.
type FailFromImpulse = NeuronFromImpulse FailNeuron
-- | 'Impulse's for 'FailNeuron'. This 'Neuron' does not define any 'Impulse's it would receive, 'NoImpulse'.
type FailForImpulse = NeuronForImpulse FailNeuron
{-| Options for 'FailNeuron'. This option is defined:

[@delay :: 'Int'@] The delay in microseconds before 'Neuron' fails. Default is no delay.
-}
type FailOptions = NeuronOptions FailNeuron

-- | A simple 'Neuron' which just fails in 'grow'ing phase.
instance Neuron FailNeuron where
  type NeuronFromImpulse FailNeuron = NoImpulse
  type NeuronForImpulse FailNeuron = NoImpulse
  data NeuronOptions FailNeuron = FailOptions {
      delay :: Int
    } deriving (Eq, Ord, Read, Show, Data)
  
  mkDefaultOptions = return FailOptions {
      delay = defaultDelay
    }
  
  grow FailOptions { delay } = do
    threadDelay delay
    dissolving (undefined :: FailNeuron)
