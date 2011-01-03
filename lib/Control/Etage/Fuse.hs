{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns #-}

module Control.Etage.Fuse (
  fuse,
  FuseFromImpulse,
  FuseForImpulse
) where

import Control.Applicative
import Control.Monad
import Data.Typeable

import Control.Etage.Internals
import Control.Etage.Externals

data FuseNeuron i j = FuseNeuron (FuseOptions i j) deriving (Typeable)

type FuseFromImpulse i j = NeuronFromImpulse (FuseNeuron i j)
type FuseForImpulse i j = NeuronForImpulse (FuseNeuron i j)
type FuseOptions i j = NeuronOptions (FuseNeuron i j)

{-|
An internal 'Neuron' which implements 'fuse'.
-}
instance (Impulse i, Impulse j) => Neuron (FuseNeuron i j) where
  type NeuronFromImpulse (FuseNeuron i j) = j
  type NeuronForImpulse (FuseNeuron i j) = NoImpulse
  data NeuronOptions (FuseNeuron i j) = FuseOptions {
      fuser :: ImpulseTime -> [i] -> [j],
      nerves :: [TranslatableFrom i]
    }
  
  mkDefaultOptions = return FuseOptions {
      fuser = undefined,
      nerves = undefined
    }
  
  grow options = return $ FuseNeuron options
  
  live nerve (FuseNeuron FuseOptions { fuser, nerves }) = forever $ do
    is <- concat <$> mapM (\(TranslatableFrom n) -> translate <$> getFromNeuron n) nerves
    time <- getCurrentImpulseTime
    sendListFromNeuron nerve $ fuser time is

{-|
It 'grow's an internal 'Neuron' which 'fuse's 'Impulse's received from given 'Nerve's using the given function, sending them over
the resulting 'grow'n 'Nerve', 'translate'-ing received 'Impulse's as necessary.

The important aspect of 'fuse'-ing is its synchronization behavior, as it requires exactly one 'Impulse' from each given 'Nerve' at
a time to 'fuse' them together. So it is important that all given 'Nerve's have more or less the equal density of 'Impulse's, otherwise
queues of some 'Nerve's will grow unproportionally because of the stalled 'Impulse's, causing at least a memory leak.

'impulseFuser' helper function can maybe help you with defining fusing function. 'fuseWith' uses type of the given function to construct
type of the resulting 'Nerve' so probably too polymorphic type will give you problems.

Check 'fuseWith' for a more high-level function (of 'Incubation') taking care of all the details (like branching 'Nerve's as necessary).
Use this function only if you are dealing with 'grow'ing and 'attach'ing of 'Nerve's directly.
-}
fuse :: forall i j. (Impulse i, Impulse j) => [TranslatableFrom i] -> (ImpulseTime -> [i] -> [j]) -> IO (Nerve (FuseFromImpulse i j) AxonConductive (FuseForImpulse i j) AxonNonConductive)
fuse nerves fuser = do
  -- we do not manage this neuron, it will be cleaned by RTS at program exit
  -- TODO: What if this is not the only thing the program is doing? Should we cleanup this threads at the end of Incubation, too?
  nerve <- growNerve
  _ <- attach (\o -> o { fuser, nerves } :: NeuronOptions (FuseNeuron i j)) nerve
  return nerve
