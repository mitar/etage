{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns #-}

{-|
This module defines a simple 'Neuron' which initiates 'dissolving' after a given delay. It can be used to limit execution time of
the network. You 'grow' it in 'Incubation' by using something like:

> _ <- (growNeuron :: NerveNone TimeoutNeuron) (\o -> o { timeout = 10000000 })

somewhere among (best at the end) 'growNeuron' calls for other 'Neuron's in 'Incubation'.

It is an example of a 'Neuron' which does not 'live' indefinitely (until an exception) but 'dissolve's after some time (by using
'dissolving'). It does not process any 'Impulse's.
-}

module Control.Etage.Timeout (
  TimeoutNeuron,
  TimeoutFromImpulse,
  TimeoutForImpulse,
  TimeoutOptions,
  NeuronOptions(..)
) where

import Control.Concurrent
import Data.Data

import Control.Etage

defaultTimeout :: Int
defaultTimeout = 60000000 -- microseconds, 60 seconds

data TimeoutNeuron = TimeoutNeuron TimeoutOptions deriving (Typeable, Data)

instance Show TimeoutNeuron where
  show = show . typeOf

-- | 'Impulse's from 'TimeoutNeuron'. This 'Neuron' does not define any 'Impulse's it would send, 'NoImpulse'.
type TimeoutFromImpulse = NeuronFromImpulse TimeoutNeuron
-- | 'Impulse's for 'TimeoutNeuron'. This 'Neuron' does not define any 'Impulse's it would receive, 'NoImpulse'.
type TimeoutForImpulse = NeuronForImpulse TimeoutNeuron
{-|
Options for 'TimeoutNeuron'. This option is defined:

[@timeout :: 'Int'@] The length of the delay in microseconds before initiating 'dissolving'. Default is 60 seconds.
-}
type TimeoutOptions = NeuronOptions TimeoutNeuron

-- | A simple 'Neuron' which initiates 'dissolving' after a given delay.
instance Neuron TimeoutNeuron where
  type NeuronFromImpulse TimeoutNeuron = NoImpulse
  type NeuronForImpulse TimeoutNeuron = NoImpulse
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
