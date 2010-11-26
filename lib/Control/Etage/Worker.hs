{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Worker (
  WorkerNeuron,
  WorkerFromImpulse,
  WorkerForImpulse,
  WorkerOptions,
  NeuronFromImpulse,
  NeuronForImpulse(..),
  NeuronOptions(..),
  WorkType
) where

import Control.Applicative
import Control.Monad
import Data.Typeable

import Control.Etage

type WorkType = IO ()

instance Show WorkType where
  show _ = "Work"

data WorkerNeuron deriving (Typeable)

type WorkerFromImpulse = NeuronFromImpulse WorkerNeuron
type WorkerForImpulse = NeuronForImpulse WorkerNeuron
type WorkerOptions = NeuronOptions WorkerNeuron

instance Impulse WorkerFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

instance Impulse WorkerForImpulse where
  impulseTime Work { .. } = impulseTimestamp
  impulseValue _ = []

deriving instance Show WorkerFromImpulse

instance Neuron WorkerNeuron where
  data LiveNeuron WorkerNeuron = LiveWorkerNeuron NeuronDissolved NeuronId
  data NeuronFromImpulse WorkerNeuron
  data NeuronForImpulse WorkerNeuron = Work {
      impulseTimestamp :: ImpulseTime,
      work :: WorkType
    } deriving (Show)
  data NeuronOptions WorkerNeuron = WorkerOptions {
      mapOnCapability :: NeuronMapCapability
    } deriving (Eq, Ord, Read, Show)
  
  mkLiveNeuron = LiveWorkerNeuron
  getNeuronDissolved (LiveWorkerNeuron dissolved _) = dissolved
  getNeuronId (LiveWorkerNeuron _ nid) = nid
  
  mkDefaultOptions = do
    neuronMapCapability <- mkNeuronMapOnRandomCapability
    return WorkerOptions {
        mapOnCapability = neuronMapCapability
      }
  
  getNeuronMapCapability WorkerOptions { mapOnCapability } = mapOnCapability
  
  live nerve _ = forever $ do
    Work { .. } <- head <$> waitAndSlurpForNeuron nerve
    work
