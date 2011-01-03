{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, DeriveDataTypeable, NamedFieldPuns, DisambiguateRecordFields #-}

{-|
This module defines a 'Neuron' which delays received 'Impulse's before sending them further. In this way network can have a simple
kind of memory (state) without a need of special 'Neuron's.
-}

module Control.Etage.Delay (
  DelayNeuron,
  DelayFromImpulse,
  DelayForImpulse,
  DelayOptions,
  NeuronOptions(..)
) where

import Data.Data

import Control.Etage

defaultDelay :: Int
defaultDelay = 1

data DelayNeuron i = DelayNeuron (DelayOptions i) deriving (Typeable, Data)

-- | 'Impulse's from 'DelayNeuron', of type @i@.
type DelayFromImpulse i = NeuronFromImpulse (DelayNeuron i)
-- | 'Impulse's for 'DelayNeuron', of type @i@.
type DelayForImpulse i = NeuronForImpulse (DelayNeuron i)
{-|
Options for 'DelayNeuron'. This option is defined:

[@delay :: 'Int'@] For how many 'Impulse's should received 'Impulse's be delayed before sending them. Default value is 1.
-}
type DelayOptions i = NeuronOptions (DelayNeuron i)

-- | A 'Neuron' which delays received 'Impulse's before sending them further.
instance Impulse i => Neuron (DelayNeuron i) where
  type NeuronFromImpulse (DelayNeuron i) = i
  type NeuronForImpulse (DelayNeuron i) = i
  data NeuronOptions (DelayNeuron i) = DelayOptions {
      delay :: Int
    } deriving (Eq, Ord, Read, Show, Data)
  
  mkDefaultOptions = return DelayOptions {
      delay = defaultDelay
    }
  
  grow options = return $ DelayNeuron options
  
  live nerve (DelayNeuron DelayOptions { delay }) = live' []
    where live' pastImpulses = do
            is <- waitAndSlurpForNeuron nerve -- we want all not just newest
            let allImpulses = is ++ pastImpulses
                (delayedImpulses, readyImpulses) = splitAt delay allImpulses
            sendListFromNeuron nerve $ reverse readyImpulses
            live' delayedImpulses
