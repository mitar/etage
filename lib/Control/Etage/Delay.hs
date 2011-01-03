{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, DeriveDataTypeable, NamedFieldPuns, DisambiguateRecordFields #-}

{-|
This module defines a 'Neuron' which delays received 'Impulse's before sending them further. In this way network can have a simple
kind of memory (state) without a need of special 'Neuron's. You 'grow' it in 'Incubation' by using something like:

> nerveDelay <- (growNeuron :: NerveBoth (DelayNeuron IInteger)) (\o -> o { delay = 2 })

Sometimes the same effect can be achieved by using a 'Nerve' as a queue and using 'fuseWith' (or 'fuse') to synchronize (and thus
delay) 'Impulse's. For example, the following two programs both output Fibonacci sequence:

> incubate $ do
>   nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) (\o -> o { showInsteadOfDump = True })
>   nerveDelay <- (growNeuron :: NerveBoth (DelayNeuron IInteger)) defaultOptions
>   nerveSum <- (growNeuron :: NerveBoth (FunctionNeuron IIntegerList IInteger)) (\o -> o { function = \t -> (: []) . IValue t . sum . list })
>   nerveFused <- [TranslatableFrom nerveDelay, TranslatableFrom nerveSum] `fuseWith` (listFuser :: ImpulseTime -> [IInteger] -> [IIntegerList])
>   
>   nerveSum `attachTo` [TranslatableFor nerveDelay, TranslatableFor nerveDump]
>   nerveFused `attachTo` [TranslatableFor nerveSum]
>   
>   liftIO $ do
>     t <- getCurrentImpulseTime
>     sendFromNeuron nerveSum $ IValue t 1
>     sendFromNeuron nerveDelay $ IValue t 0

> incubate $ do
>   nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) (\o -> o { showInsteadOfDump = True })
>   nerveSum <- (growNeuron :: NerveBoth (FunctionNeuron IIntegerList IInteger)) (\o -> o { function = \t -> (: []) . IValue t . sum . list })
>   
>   liftIO $ do
>     t <- getCurrentImpulseTime
>     sendFromNeuron nerveSum $ IValue t 0
>   
>   nerveSum' <- liftIO $ branchNerveFrom nerveSum
>   nerveFused <- [TranslatableFrom nerveSum, TranslatableFrom nerveSum'] `fuseWith` (listFuser :: ImpulseTime -> [IInteger] -> [IIntegerList])
>   
>   nerveSum `attachTo` [TranslatableFor nerveDump]
>   nerveFused `attachTo` [TranslatableFor nerveSum]
>   
>   liftIO $ do
>     t <- getCurrentImpulseTime
>     sendFromNeuron nerveSum $ IValue t 1

This 'Neuron' processes all 'Impulse's it receives.
-}

module Control.Etage.Delay (
  DelayNeuron,
  DelayFromImpulse,
  DelayForImpulse,
  DelayOptions,
  NeuronOptions(..)
) where

import Data.Data

import Control.Etage

defaultDelay :: Int
defaultDelay = 1

data DelayNeuron i = DelayNeuron (DelayOptions i) deriving (Typeable, Data)

-- | 'Impulse's from 'DelayNeuron', of type @i@.
type DelayFromImpulse i = NeuronFromImpulse (DelayNeuron i)
-- | 'Impulse's for 'DelayNeuron', of type @i@.
type DelayForImpulse i = NeuronForImpulse (DelayNeuron i)
{-|
Options for 'DelayNeuron'. This option is defined:

[@delay :: 'Int'@] For how many 'Impulse's should received 'Impulse's be delayed before sending them. Default value is 1.
-}
type DelayOptions i = NeuronOptions (DelayNeuron i)

-- | A 'Neuron' which delays received 'Impulse's before sending them further.
instance Impulse i => Neuron (DelayNeuron i) where
  type NeuronFromImpulse (DelayNeuron i) = i
  type NeuronForImpulse (DelayNeuron i) = i
  data NeuronOptions (DelayNeuron i) = DelayOptions {
      delay :: Int
    } deriving (Eq, Ord, Read, Show, Data)
  
  mkDefaultOptions = return DelayOptions {
      delay = defaultDelay
    }
  
  grow options = return $ DelayNeuron options
  
  live nerve (DelayNeuron DelayOptions { delay }) = live' []
    where live' pastImpulses = do
            is <- waitAndSlurpForNeuron nerve -- we want all not just newest
            let allImpulses = is ++ pastImpulses
                (delayedImpulses, readyImpulses) = splitAt delay allImpulses
            sendListFromNeuron nerve $ reverse readyImpulses
            live' delayedImpulses
