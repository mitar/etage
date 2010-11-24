{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Propagate (
  propagate
) where

import Control.Concurrent
import Control.Monad

import Control.Etage.Types

-- TODO: Implement delay in propagation (constant delay, random from some distribution)

data PropagateNeuron from for forConductivity = PropagateNeuron (PropagateOptions from for forConductivity)

{-|
Impulse instance for internal 'Neuron' which implements 'propagate'.
-}
instance Impulse (PropagateForImpulse from for forConductivity) where
  impulseTime _ = undefined
  impulseValue _ = undefined

{-|
Impulse instance for internal 'Neuron' which implements 'propagate'.
-}
instance Impulse (PropagateFromImpulse from for forConductivity) where
  impulseTime _ = undefined
  impulseValue _ = undefined

type LivePropagateNeuron from for forConductivity = LiveNeuron (PropagateNeuron from for forConductivity)
type PropagateForImpulse from for forConductivity = NeuronForImpulse (PropagateNeuron from for forConductivity)
type PropagateFromImpulse from for forConductivity = NeuronFromImpulse (PropagateNeuron from for forConductivity)
type PropagateOptions from for forConductivity = NeuronOptions (PropagateNeuron from for forConductivity)

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance Show (PropagateForImpulse from for forConductivity) where
  show _ = undefined

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance Show (PropagateFromImpulse from for forConductivity) where
  show _ = undefined

{-|
Internal 'Neuron' which implements 'propagate'.
-}
instance Neuron (PropagateNeuron from for forConductivity) where
  data LiveNeuron (PropagateNeuron from for forConductivity) = LivePropagateNeuron NeuronDissolved NeuronId
  data NeuronForImpulse (PropagateNeuron from for forConductivity)
  data NeuronFromImpulse (PropagateNeuron from for forConductivity)
  data NeuronOptions (PropagateNeuron from for forConductivity) = PropagateOptions {
      from :: Nerve (Chan from) AxonConductive for forConductivity,
      for ::[Translatable from]
    }
  
  mkLiveNeuron = LivePropagateNeuron
  getNeuronDissolved (LivePropagateNeuron dissolved _) = dissolved
  getNeuronId (LivePropagateNeuron _ nid) = nid
  
  mkDefaultOptions = return PropagateOptions { from = undefined, for = undefined }
  
  grow options = return $ PropagateNeuron options
  
  live _ (PropagateNeuron PropagateOptions { .. }) = forever $ do
    i <- getFromNeuron from
    mapM_ (\(Translatable n) -> translateAndSend n i) for

propagate :: forall from for forConductivity. Nerve (Chan from) AxonConductive for forConductivity -> [Translatable from] -> IO ()
propagate from for = do
  -- we do not manage this neuron, it will be cleaned by RTS at program exit
  _ <- attach (\o -> o { from, for }) undefined :: IO (LivePropagateNeuron from for forConductivity)
  return ()
