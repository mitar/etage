{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, NamedFieldPuns #-}

{-|
This module defines a worker 'Neuron' which evaluates 'IO' actions it receives. It is useful to offload lengthly 'IO' actions
into another thread. In the case of too many queued 'IO' actions they are silently dropped and only newest ones are evaluated.
You 'grow' it in 'Incubation' by using something like:

> nerveWorker <- growNeuron defaultOptions :: NerveOnlyFor WorkerNeuron
-}

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

-- | Type of work this worker 'Neuron' evaluates.
type WorkType = IO ()

instance Show WorkType where
  show = show . typeOf

-- TODO: We could maybe send results back?

data WorkerNeuron deriving (Typeable)

-- | 'Impulse's from 'WorkerNeuron'. This 'Neuron' does not define any 'Impulse's it would send.
type WorkerFromImpulse = NeuronFromImpulse WorkerNeuron
{-|
'Impulse's for 'WorkerNeuron'. This 'Impulse' constructor is defined:

[@Work { impulseTimestamp :: ImpulseTime, work :: WorkType }@]
@impulseTimestamp@ is time when the action was enqueued for evaluation in the 'WorkerNeuron', @work@ is enqueued action.
-}
type WorkerForImpulse = NeuronForImpulse WorkerNeuron
{-|
Options for 'WorkerNeuron'. This option is defined:

[@mapOnCapability :: 'NeuronMapCapability'@] How to map the 'Neuron' on capabilities (OS threads). With this option you can fix
multiple 'Neuron's on the same capability (for example, by generating one value with 'mkNeuronMapOnRandomCapability' and using
it for all those 'Neuron's) which is sometimes necessary when dealing with external (FFI) libraries. Default value
is 'NeuronFreelyMapOnCapability'.
-}
type WorkerOptions = NeuronOptions WorkerNeuron

-- | Impulse instance for 'WorkerNeuron'.
instance Impulse WorkerFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

-- | Impulse instance for 'WorkerNeuron'.
instance Impulse WorkerForImpulse where
  impulseTime Work { impulseTimestamp } = impulseTimestamp
  impulseValue _ = []

deriving instance Show WorkerFromImpulse

-- | A worker 'Neuron' which evaluates 'IO' actions it receives.
instance Neuron WorkerNeuron where
  data NeuronFromImpulse WorkerNeuron
  data NeuronForImpulse WorkerNeuron = Work {
      impulseTimestamp :: ImpulseTime,
      work :: WorkType
    } deriving (Show)
  data NeuronOptions WorkerNeuron = WorkerOptions {
      mapOnCapability :: NeuronMapCapability
    } deriving (Eq, Ord, Read, Show)
  
  mkDefaultOptions = return WorkerOptions {
      mapOnCapability = NeuronFreelyMapOnCapability
    }
  
  getNeuronMapCapability WorkerOptions { mapOnCapability } = mapOnCapability
  
  live nerve _ = forever $ do
    Work { work } <- head <$> waitAndSlurpForNeuron nerve -- just newest
    work
