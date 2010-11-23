{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Propagate (
  propagate
) where

import Control.Concurrent
import Control.Monad

import Control.Etage.Types

-- TODO: Implement delay in propagation (constant delay, random from some distribution)

data PropagateNeuron a a' c c' d = PropagateNeuron (PropagateOptions a a' c c' d)

{-|
Impulse instance for internal 'Neuron' which implements 'propagate'.
-}
instance Impulse (PropagateForImpulse a a' c c' d) where
  impulseTime _ = undefined
  impulseValue _ = undefined

{-|
Impulse instance for internal 'Neuron' which implements 'propagate'.
-}
instance Impulse (PropagateFromImpulse a a' c c' d) where
  impulseTime _ = undefined
  impulseValue _ = undefined

type LivePropagateNeuron a a' c c' d = LiveNeuron (PropagateNeuron a a' c c' d)
type PropagateForImpulse a a' c c' d = NeuronForImpulse (PropagateNeuron a a' c c' d)
type PropagateFromImpulse a a' c c' d = NeuronFromImpulse (PropagateNeuron a a' c c' d)
type PropagateOptions a a' c c' d = NeuronOptions (PropagateNeuron a a' c c' d)

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance Show (PropagateForImpulse a a' c c' d) where
  show _ = undefined

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance Show (PropagateFromImpulse a a' c c' d) where
  show _ = undefined

{-|
Internal 'Neuron' which implements 'propagate'.
-}
instance Neuron (PropagateNeuron a a' c c' d) where
  data LiveNeuron (PropagateNeuron a a' c c' d) = LivePropagateNeuron NeuronDissolved NeuronId
  data NeuronForImpulse (PropagateNeuron a a' c c' d)
  data NeuronFromImpulse (PropagateNeuron a a' c c' d)
  data NeuronOptions (PropagateNeuron a a' c c' d) = PropagateOptions {
      from :: Nerve (Chan a) a' AxonConductive c c' d,
      for ::[Translatable a']
    }
  
  mkLiveNeuron = LivePropagateNeuron
  getNeuronDissolved (LivePropagateNeuron dissolved _) = dissolved
  getNeuronId (LivePropagateNeuron _ nid) = nid
  
  mkDefaultOptions = return PropagateOptions { from = undefined, for = undefined }
  
  grow options = return $ PropagateNeuron options
  
  live _ (PropagateNeuron PropagateOptions { .. }) = forever $ do
    i <- getFromNeuron from
    mapM_ (\(Translatable n) -> translateAndSend n i) for

propagate :: forall a a' c c' d. Nerve (Chan a) a' AxonConductive c c' d -> [Translatable a'] -> IO ()
propagate from for = do
  -- we do not manage this neuron, it will be cleaned by RTS at program exit
  _ <- attach (\o -> o { from, for }) undefined :: IO (LivePropagateNeuron a a' c c' d)
  return ()
