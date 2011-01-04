{-# LANGUAGE TypeFamilies, GADTs, ScopedTypeVariables, TypeSynonymInstances, DeriveDataTypeable, NamedFieldPuns #-}

module Control.Etage.Propagate (
  propagate
) where

import Control.Monad
import Data.Typeable

import Control.Etage.Internals
import Control.Etage.Externals

-- TODO: Implement delay in propagation (constant delay, random from some distribution)

data PropagateNeuron from for = PropagateNeuron (PropagateOptions from for) deriving (Typeable)

type PropagateOptions from for = NeuronOptions (PropagateNeuron from for)

{-|
An internal 'Neuron' which implements 'propagate'.
-}
instance (Impulse from, Impulse for) => Neuron (PropagateNeuron from for) where
  type NeuronFromImpulse (PropagateNeuron from for) = from
  type NeuronForImpulse (PropagateNeuron from for) = for
  data NeuronOptions (PropagateNeuron from for) = PropagateOptions {
      for ::[TranslatableFor for]
    }
  
  mkDefaultOptions = return PropagateOptions {
      for = undefined
    }
  
  grow options = return $ PropagateNeuron options
  
  live nerve (PropagateNeuron PropagateOptions { for }) = forever $ do
    i <- getForNeuron nerve
    mapM_ (\(TranslatableFor n) -> translateAndSend n i) for

{-|
It 'grow's an internal 'Neuron' which 'propagate's 'Impulse's from a given 'Nerve' to other 'Nerve's, 'translate'-ing as necessary.

Be careful if you are 'propagate'-ing the same 'Nerve' multiple times as some 'Impulse's might already been 'propagate'd and thus are not
available anymore to later 'propagate'd 'Nerve's. Just list all destination 'Nerve's the first time.

Check 'attachTo' for a more high-level function (of 'Incubation') taking care of all the details (like branching 'Nerve's as necessary).
Use this function only if you are dealing with 'grow'ing and 'attach'ing of 'Nerve's directly.
-}
propagate :: forall from for forConductivity. (Impulse from, Impulse for) => Nerve from AxonConductive for forConductivity -> [TranslatableFor from] -> IO ()
propagate _ [] = return ()
propagate from for = do
  -- we do not manage this neuron, it will be cleaned by RTS at program exit
  -- TODO: What if this is not the only thing the program is doing? Should we cleanup this threads at the end of Incubation, too?
  _ <- attach (\o -> o { for } :: NeuronOptions (PropagateNeuron for from)) (cross from) -- we use cross here so that in neuron we can behave as in normal neuron (use getForNeuron for example)
  return ()
