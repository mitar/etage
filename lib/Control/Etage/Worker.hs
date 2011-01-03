{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, NamedFieldPuns, DisambiguateRecordFields #-}

{-|
This module defines a worker 'Neuron' which evaluates 'IO' actions it receives. It is useful to offload lengthly 'IO' actions
into another thread. In the case of too many queued 'IO' actions they are silently dropped and only newest ones are evaluated.
You 'grow' it in 'Incubation' by using something like:

> nerveWorker <- (growNeuron :: NerveOnlyFor WorkerNeuron) defaultOptions

It is an example of a 'Neuron' which defines 'getNeuronMapCapability'. It processes only the newest 'Impulse's it receives, when
they get queued, so 'Impulse's are dropped if load is too high.
-}

module Control.Etage.Worker (
  WorkerNeuron,
  WorkerFromImpulse,
  WorkerForImpulse(..),
  WorkerOptions,
  NeuronOptions(..),
  WorkType
) where

import Control.Applicative
import Control.Monad
import Data.Data

import Control.Etage

-- | Type of work this worker 'Neuron' evaluates.
type WorkType = IO ()

instance Show WorkType where
  show = show . typeOf

-- TODO: We could maybe send results back?

data WorkerNeuron deriving (Typeable)

deriving instance Data WorkerNeuron

-- | 'Impulse's from 'WorkerNeuron'. This 'Neuron' does not define any 'Impulse's it would send, 'NoImpulse'.
type WorkerFromImpulse = NeuronFromImpulse WorkerNeuron
-- | 'Impulse's for 'WorkerNeuron'.
data WorkerForImpulse = Work {
    impulseTimestamp :: ImpulseTime, -- ^ Time when the action was enqueued for evaluation in the 'WorkerNeuron'.
    work :: WorkType -- ^ Enqueued action.
  } deriving (Show, Typeable)
{-|
Options for 'WorkerNeuron'. This option is defined:

[@mapOnCapability :: 'NeuronMapCapability'@] How to map the 'Neuron' on capabilities (OS threads). With this option you can fix
multiple 'Neuron's on the same capability (for example, by generating one value with 'mkNeuronMapOnRandomCapability' and using
it for all those 'Neuron's) which is sometimes necessary when dealing with external (FFI) libraries. Default value
is 'NeuronFreelyMapOnCapability'.
-}
type WorkerOptions = NeuronOptions WorkerNeuron

instance Impulse WorkerForImpulse where
  impulseTime Work { impulseTimestamp } = impulseTimestamp
  impulseValue _ = []

-- | A worker 'Neuron' which evaluates 'IO' actions it receives.
instance Neuron WorkerNeuron where
  type NeuronFromImpulse WorkerNeuron = NoImpulse
  type NeuronForImpulse WorkerNeuron = WorkerForImpulse
  data NeuronOptions WorkerNeuron = WorkerOptions {
      mapOnCapability :: NeuronMapCapability
    } deriving (Eq, Ord, Read, Show, Data)
  
  mkDefaultOptions = return WorkerOptions {
      mapOnCapability = NeuronFreelyMapOnCapability
    }
  
  getNeuronMapCapability WorkerOptions { mapOnCapability } = mapOnCapability
  
  live nerve _ = forever $ do
    Work { work } <- head <$> waitAndSlurpForNeuron nerve -- just newest
    work
