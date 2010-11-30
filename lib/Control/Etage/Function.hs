{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns, BangPatterns #-}

{-|
This module defines a 'Neuron' which applies a given function to received 'Impulse's. As Haskell is a lazy language this does
not mean that the result will be immediately evaluated but that it will be evaluated when (and if) the result will be needed
(probably in some other 'Neuron'). You 'grow' it in 'Incubation' by using something like:

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
import Data.Typeable

import Control.Etage

defaultFunction :: [Rational] -> Rational
defaultFunction = sum

data FunctionNeuron = FunctionNeuron FunctionOptions deriving (Typeable)

instance Show FunctionNeuron where
  show = show . typeOf

{-|
'Impulse's from 'FunctionNeuron'. This 'Impulse' constructor is defined:

[@Value { impulseTimestamp :: 'ImpulseTime', value :: 'Rational' }@]
@impulseTimestamp@ is time when the function was applied (but not when the result was evaluated), @value@ contains the (lazy) result.
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

-- | A 'Neuron' which applies a given function to received 'Impulse's.
instance Neuron FunctionNeuron where
  data NeuronFromImpulse FunctionNeuron = Value {
      impulseTimestamp :: ImpulseTime, -- time is first so that ordering is first by time
      value :: Rational
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
    let r = function . impulseValue $ i
    time <- getCurrentImpulseTime
    sendFromNeuron nerve Value { impulseTimestamp = time, value = r }

instance Impulse i => ImpulseTranslator i FunctionForImpulse where
  translate i = [FunctionForImpulse i]
