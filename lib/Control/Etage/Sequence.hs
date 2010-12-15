{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, NamedFieldPuns #-}

{-|
This module defines a 'Neuron' which generates values based on a given sequence at a given interval.
If it reaches the end of a sequence it initiates 'dissolving'.
You 'grow' default version of it, which gives you an infinite source of random 'Int's at random interval of maximum length of 1
second, in 'Incubation' by using something like:

> nerveRandom <- growNeuron defaultOptions :: NerveOnlyFrom (SequenceNeuron Int)

or for an infinite source of ones with same random interval:

> nerveOnes <- growNeuron (\o -> o { valueSource = repeat 1 }) :: NerveOnlyFrom (SequenceNeuron Int)

It is an example of a 'Neuron' with a parametrized type.
-}

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
import Data.Data
import System.Random

import Control.Etage

defaultMaxInterval :: Int
defaultMaxInterval = 1000000 -- microseconds, 1 second

data SequenceNeuron r = SequenceNeuron (SequenceOptions r) deriving (Typeable, Data)

instance Typeable r => Show (SequenceNeuron r) where
  show = show . typeOf

{-|
'Impulse's from 'SequenceNeuron'. This 'Impulse' constructor is defined:

[@Value { impulseTimestamp :: 'ImpulseTime', value :: 'Rational' }@]
@impulseTimestamp@ is time when the value was send, @value@ contains the value.
-}
type SequenceFromImpulse r = NeuronFromImpulse (SequenceNeuron r)
-- | 'Impulse's for 'SequenceNeuron'. This 'Neuron' does not define any 'Impulse's it would receive.
type SequenceForImpulse r = NeuronForImpulse (SequenceNeuron r)
{-|
Options for 'SequenceNeuron'. Those options are defined:

[@valueSource :: \[r\]@] The list of values to send. If the end of the list is reached, 'Neuron' initiates 'dissolving'. Default
is an infinite list of values of type @r@ generated by the 'StdGen' random generator.

[@intervalSource :: \['Int'\]@] The list of intervals between values. It is defined as a delay in microseconds before the next value
is send. If the end of the list is reached, 'Neuron' initiates 'dissolving'. Default is a list of random delays with maximum
length of 1 second generated by the 'StdGen' random generator.
-}
type SequenceOptions r = NeuronOptions (SequenceNeuron r)

-- | Impulse instance for 'SequenceNeuron'.
instance (Real r, Random r, Show r, Typeable r) => Impulse (SequenceFromImpulse r) where
  impulseTime Value { impulseTimestamp } = impulseTimestamp
  impulseValue Value { value } = [toRational value]

-- | Impulse instance for 'SequenceNeuron'.
instance (Real r, Random r, Show r, Typeable r) => Impulse (SequenceForImpulse r) where
  impulseTime _ = undefined
  impulseValue _ = undefined

deriving instance Show (SequenceForImpulse r)

deriving instance Data r => Data (SequenceForImpulse r)

-- | A 'Neuron' which generates values based on a given sequence at a given interval.
instance (Real r, Random r, Show r, Typeable r) => Neuron (SequenceNeuron r) where
  data NeuronFromImpulse (SequenceNeuron r) = Value {
      impulseTimestamp :: ImpulseTime, -- time is first so that ordering is first by time
      value :: r
    } deriving (Eq, Ord, Read, Show, Data)
  data NeuronForImpulse (SequenceNeuron r)
  data NeuronOptions (SequenceNeuron r) = SequenceOptions {
      valueSource :: [r],
      intervalSource :: [Int] -- microseconds
    } deriving (Eq, Ord, Read, Show, Data)
  
  mkDefaultOptions = do
    generator <- newStdGen
    generator' <- newStdGen
    return SequenceOptions {
        valueSource = randoms generator,
        intervalSource = randomRs (0, defaultMaxInterval) generator'
      }
  
  grow options = return $ SequenceNeuron options
  
  live nerve n@(SequenceNeuron SequenceOptions { valueSource, intervalSource }) = do
    forM_ (zip valueSource intervalSource) $ \(v, i) -> do
      threadDelay i
      time <- getCurrentImpulseTime
      sendFromNeuron nerve $ Value time v
    dissolving n
