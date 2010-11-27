{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, StandaloneDeriving, NamedFieldPuns #-}

module Control.Etage.Fail (
  FailNeuron,
  FailFromImpulse,
  FailForImpulse,
  FailOptions,
  NeuronFromImpulse,
  NeuronForImpulse,
  NeuronOptions
) where

import Data.Typeable

import Control.Etage

data FailNeuron deriving (Typeable)

instance Show FailNeuron where
  show = show . typeOf

type FailFromImpulse = NeuronFromImpulse FailNeuron
type FailForImpulse = NeuronForImpulse FailNeuron
type FailOptions = NeuronOptions FailNeuron

instance Impulse FailFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

instance Impulse FailForImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

deriving instance Show FailFromImpulse
deriving instance Show FailForImpulse

instance Neuron FailNeuron where
  data NeuronFromImpulse FailNeuron
  data NeuronForImpulse FailNeuron
  data NeuronOptions FailNeuron
  
  grow _ = dissolving (undefined :: FailNeuron)
