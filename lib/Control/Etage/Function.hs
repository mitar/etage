{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, NamedFieldPuns, BangPatterns #-}

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

type FunctionFromImpulse = NeuronFromImpulse FunctionNeuron
type FunctionForImpulse = NeuronForImpulse FunctionNeuron
type FunctionOptions = NeuronOptions FunctionNeuron

instance Impulse FunctionFromImpulse where
  impulseTime Value { impulseTimestamp } = impulseTimestamp
  impulseValue Value { value } = [value]

instance Impulse FunctionForImpulse where
  impulseTime (FunctionForImpulse i) = impulseTime i
  impulseValue (FunctionForImpulse i) = impulseValue i

deriving instance Show FunctionForImpulse

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
    sendFromNeuron nerve $ Value { impulseTimestamp = time2, value = r, evaluationTime = time2 - time1 }

instance Impulse i => ImpulseTranslator i FunctionForImpulse where
  translate i = [FunctionForImpulse i]
