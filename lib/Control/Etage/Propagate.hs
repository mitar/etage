{-# LANGUAGE TypeFamilies, GADTs, ScopedTypeVariables, TypeSynonymInstances, DeriveDataTypeable, NamedFieldPuns #-}

module Control.Etage.Propagate (
  propagate,
  Translatable(..)
) where

import Control.Monad
import Data.Typeable

import Control.Etage.Internals
import Control.Etage.Externals

-- TODO: Implement delay in propagation (constant delay, random from some distribution)

data (Typeable from, Typeable for, Typeable forConductivity) => PropagateNeuron from for forConductivity = PropagateNeuron (PropagateOptions from for forConductivity) deriving (Typeable)

type PropagateOptions from for forConductivity = NeuronOptions (PropagateNeuron from for forConductivity)

{-|
An internal 'Neuron' which implements 'propagate'.
-}
instance (Typeable from, Typeable for, Typeable forConductivity) => Neuron (PropagateNeuron from for forConductivity) where
  type NeuronFromImpulse (PropagateNeuron from for forConductivity) = NoImpulse
  type NeuronForImpulse (PropagateNeuron from for forConductivity) = NoImpulse
  data NeuronOptions (PropagateNeuron from for forConductivity) = PropagateOptions {
      from :: Nerve from AxonConductive for forConductivity,
      for ::[Translatable from]
    }
  
  mkDefaultOptions = return PropagateOptions {
      from = undefined,
      for = undefined
    }
  
  grow options = return $ PropagateNeuron options
  
  live _ (PropagateNeuron PropagateOptions { from, for }) = forever $ do
    i <- getFromNeuron from
    mapM_ (\(Translatable n) -> translateAndSend n i) for

{-|
It 'grow's an internal 'Neuron' which propagates 'Impulse's from a given 'Nerve' to other 'Nerve's, 'translate'-ing as necessary.

Check 'attachTo' for a more high-level function (of 'Incubation') taking care of all the details (like branching 'Nerve's as necessary).
Use this function only if you are dealing with 'grow'ing and 'attach'ing of 'Nerve's directly.
-}
propagate :: forall from for forConductivity. (Typeable from, Typeable for, Typeable forConductivity) => Nerve from AxonConductive for forConductivity -> [Translatable from] -> IO ()
propagate from for = do
  -- we do not manage this neuron, it will be cleaned by RTS at program exit
  -- TODO: What if this is not the only thing the program is doing? Should we cleanup this threads at the end of Incubation, too?
  _ <- attach (\o -> o { from, for } :: NeuronOptions (PropagateNeuron from for forConductivity)) undefined
  return ()

{-|
An existentially quantified type encompassing all 'Nerve's which can be 'translate'd from the same 'Impulse' type. Used in 'attachTo'
(and 'propagate') to list all 'Nerve's to which you want a given 'Nerve' to 'attach' to (and 'Impulse's to 'propagate').
-}
data Translatable i where
  Translatable :: ImpulseTranslator i for => Nerve from fromConductivity for AxonConductive -> Translatable i
