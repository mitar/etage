{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns #-}

{-|
This module defines a 'Neuron' which generates values based on a given sequence at a given interval.
If it reaches the end of a sequence it initiates 'dissolving'.
You 'grow' default version of it, which gives you an infinite source of random 'Int's at random interval of maximum length of 1
second, in 'Incubation' by using something like:

> nerveRandom <- (growNeuron :: NerveOnlyFrom (SequenceNeuron Int)) defaultOptions

or for an infinite source of ones with same random interval:

> nerveOnes <- (growNeuron :: NerveOnlyFrom (SequenceNeuron Int)) (\o -> o { valueSource = repeat 1 })

It is an example of a 'Neuron' with a parametrized type. Check also "Control.Etage.Function" for a 'Neuron' with both receiving
and sending 'Impulse's types parametrized.
-}

module Control.Etage.Sequence (
  SequenceNeuron,
  SequenceFromImpulse,
  SequenceForImpulse,
  SequenceOptions,
  NeuronOptions(..)
) where

import Control.Concurrent
import Control.Monad
import Data.Data
import System.Random

import Control.Etage

defaultMaxInterval :: Int
defaultMaxInterval = 1000000 -- microseconds, 1 second

data SequenceNeuron v = SequenceNeuron (SequenceOptions v) deriving (Typeable, Data)

instance Typeable v => Show (SequenceNeuron v) where
  show = show . typeOf

{-|
'Impulse's from 'SequenceNeuron', @'IValue' v@.
-}
type SequenceFromImpulse v = NeuronFromImpulse (SequenceNeuron v)
-- | 'Impulse's for 'SequenceNeuron'. This 'Neuron' does not define any 'Impulse's it would receive, 'NoImpulse'.
type SequenceForImpulse v = NeuronForImpulse (SequenceNeuron v)
{-|
Options for 'SequenceNeuron'. Those options are defined:

[@valueSource :: \[v\]@] The list of values to send. If the end of the list is reached, 'Neuron' initiates 'dissolving'. Default
is an infinite list of values of type @v@ generated by the 'StdGen' random generator.

[@intervalSource :: \['Int'\]@] The list of intervals between values. It is defined as a delay in microseconds before the next value
is send. If the end of the list is reached, 'Neuron' initiates 'dissolving'. Default is a list of random delays with maximum
length of 1 second generated by the 'StdGen' random generator.
-}
type SequenceOptions v = NeuronOptions (SequenceNeuron v)

-- | A 'Neuron' which generates values based on a given sequence at a given interval.
instance (Real v, Random v, Show v, Typeable v) => Neuron (SequenceNeuron v) where
  type NeuronFromImpulse (SequenceNeuron v) = IValue v
  type NeuronForImpulse (SequenceNeuron v) = NoImpulse
  data NeuronOptions (SequenceNeuron v) = SequenceOptions {
      valueSource :: [v],
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
      sendFromNeuron nerve $ IValue time v
    dissolving n
