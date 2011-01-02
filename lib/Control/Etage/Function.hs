{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns, DisambiguateRecordFields #-}

{-|
This module defines a 'Neuron' which applies a given function to received 'Impulse's. As Haskell is a lazy language this does
not mean that the result will be immediately evaluated but that it will be evaluated when (and if) the result will be needed
(probably in some other 'Neuron'). You 'grow' it in 'Incubation' by using something like:

> nerveFunction <- (growNeuron :: NerveBoth FunctionNeuron) (\o -> o { function = negate . sum })

It is an example of a 'Neuron' which can operate on any 'Impulse' type by using 'impulseValue' type class method.
-}

module Control.Etage.Function (
  FunctionNeuron,
  FunctionFromImpulse,
  FunctionForImpulse,
  FunctionOptions,
  NeuronOptions(..)
) where

import Control.Applicative
import Control.Monad
import Data.Data

import Control.Etage

defaultFunction :: [Rational] -> Rational
defaultFunction = sum

data FunctionNeuron = FunctionNeuron FunctionOptions deriving (Typeable)

instance Show FunctionNeuron where
  show = show . typeOf

{-|
'Impulse's from 'FunctionNeuron', 'IRational'.
-}
type FunctionFromImpulse = NeuronFromImpulse FunctionNeuron
-- | 'Impulse's for 'FunctionNeuron'. This 'Neuron' can recieve any 'Impulse' type, 'AnyImpulse'.
type FunctionForImpulse = NeuronForImpulse FunctionNeuron
{-|
Options for 'FunctionNeuron'. This option is defined:

[@function :: \['Rational'\] -> 'Rational'@] The function to apply to recieved 'Impulse's. Default is 'sum'.
-}
type FunctionOptions = NeuronOptions FunctionNeuron

-- | A 'Neuron' which applies a given function to received 'Impulse's.
instance Neuron FunctionNeuron where
  type NeuronFromImpulse FunctionNeuron = IRational
  type NeuronForImpulse FunctionNeuron = AnyImpulse
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
    sendFromNeuron nerve IValue { impulseTimestamp = time, value = r }
