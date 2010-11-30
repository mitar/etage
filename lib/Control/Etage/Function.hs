{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns, BangPatterns #-}

{-|
This module defines a 'Neuron' which sends results of applying a given function to recieved 'Impulse's. You 'grow' it in
'Incubation' by using something like:

> nerveFunction <- growNeuron (\o -> o { function = negate . sum }) :: NerveBoth FunctionNeuron

It is an example of a 'Neuron' which can operate on any 'Impulse' type by using 'impulseValue' type class method.
-}

module Control.Etage.Function (
  FunctionNeuron,
  FunctionFromImpulse,
  FunctionForImpulse,
  FunctionOptions,
  NeuronFromImpulse,
  NeuronForImpulse,
  NeuronOptions(..)
) where

import Control.Applicative
import Control.Monad
import Data.Time.Clock
import Data.Typeable

import Control.Etage

defaultFunction :: [Rational] -> Rational
defaultFunction = sum

data FunctionNeuron = FunctionNeuron FunctionOptions deriving (Typeable)

instance Show FunctionNeuron where
  show = show . typeOf

{-|
'Impulse's from 'FunctionNeuron'. This 'Impulse' constructor is defined:

[@Value { impulseTimestamp :: 'ImpulseTime', value :: 'Rational', evaluationTime :: 'NominalDiffTime' }@]
@impulseTimestamp@ is time when the result was evaluated, @value@ contains the evaluated result, @evaluationTime@ is how long the
evaluation took.
-}
type FunctionFromImpulse = NeuronFromImpulse FunctionNeuron
-- | 'Impulse's for 'FunctionNeuron'. This 'Neuron' can recieve any 'Impulse' type.
type FunctionForImpulse = NeuronForImpulse FunctionNeuron
{-|
Options for 'FunctionNeuron'. This option is defined:

[@function :: \['Rational'\] -> 'Rational'@] The function to apply to recieved 'Impulse's. Default is 'sum'.
-}
type FunctionOptions = NeuronOptions FunctionNeuron

-- | Impulse instance for 'FunctionNeuron'.
instance Impulse FunctionFromImpulse where
  impulseTime Value { impulseTimestamp } = impulseTimestamp
  impulseValue Value { value } = [value]

-- | Impulse instance for 'FunctionNeuron'.
instance Impulse FunctionForImpulse where
  impulseTime (FunctionForImpulse i) = impulseTime i
  impulseValue (FunctionForImpulse i) = impulseValue i

instance Show FunctionForImpulse where
  show (FunctionForImpulse i) = show i

instance Eq FunctionForImpulse where
  (==) = impulseEq

instance Ord FunctionForImpulse where
  compare = impulseCompare

-- | A 'Neuron' which sends results of a given function for recieved 'Impulse's.
instance Neuron FunctionNeuron where
  data NeuronFromImpulse FunctionNeuron = Value {
      impulseTimestamp :: ImpulseTime, -- time is first so that ordering is first by time
      value :: Rational,
      evaluationTime :: NominalDiffTime
    } deriving (Eq, Ord, Read, Show)
  data NeuronForImpulse FunctionNeuron where
    FunctionForImpulse :: Impulse i => i -> FunctionForImpulse
  data NeuronOptions FunctionNeuron = FunctionOptions {
      function :: [Rational] -> Rational
    }
  
  mkDefaultOptions = return FunctionOptions {
      function = defaultFunction
    }
  
  grow options = return $ FunctionNeuron options
  
  live nerve (FunctionNeuron FunctionOptions { function }) = forever $ do
    i <- head <$> waitAndSlurpForNeuron nerve -- just newest
    time1 <- getCurrentImpulseTime
    let !r = function . impulseValue $ i
    time2 <- getCurrentImpulseTime
    sendFromNeuron nerve Value { impulseTimestamp = time2, value = r, evaluationTime = time2 - time1 }

instance Impulse i => ImpulseTranslator i FunctionForImpulse where
  translate i = [FunctionForImpulse i]
