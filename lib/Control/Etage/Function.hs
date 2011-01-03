{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns, DisambiguateRecordFields #-}

{-|
This module defines a 'Neuron' which applies a given function to received 'Impulse's. As Haskell is a lazy language this does
not mean that the result will be immediately (fully) evaluated but that it will be evaluated when (and if) the result will be
needed (probably in some other 'Neuron'). You 'grow' it in 'Incubation' by using something like:

> nerveFunction <- (growNeuron :: NerveBoth (FunctionNeuron AnyImpulse IRational)) (\o -> o { function = \t -> (: []) . IValue t . sum . impulseValue })

This example can receive any 'Impulse' type ('AnyImpulse') and returns 'sum' of its data payload (as given by 'impulseValue')
as 'IRational' type.

The following example calculates the greatest common divisor ('gcd'):

> incubate $ do
>   let gcd t IList { list = (a:b:is) } = let r = a `mod` b in if r == 0 then [IList t (b:is)] else [IList t (b:r:is)]
>       gcd _ _ = []
>   
>   nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) (\o -> o { showInsteadOfDump = True })
>   nerveSum <- (growNeuron :: NerveBoth (FunctionNeuron IIntegerList IIntegerList)) (\o -> o { function = gcd })
>   
>   nerveSum `attachTo` [TranslatableFor nerveSum, TranslatableFor nerveDump]
>   
>   liftIO $ do
>     t <- getCurrentImpulseTime
>     sendForNeuron nerveSum $ IList t [110, 80, 5]

This 'Neuron' is an example of a 'Neuron' with both receiving and sending 'Impulse's types parametrized. It processes only the newest 'Impulse's it receives, when
they get queued, so 'Impulse's are dropped if load is too high.
-}

module Control.Etage.Function (
  FunctionNeuron,
  FunctionFromImpulse,
  FunctionForImpulse,
  FunctionOptions,
  NeuronOptions(..)
) where

import Control.Applicative
import Control.Monad
import Data.Data

import Control.Etage

defaultFunction :: (Impulse i, Impulse j) => ImpulseTime -> i -> [j]
defaultFunction _ _ = []

data FunctionNeuron i j = FunctionNeuron (FunctionOptions i j) deriving (Typeable)

instance (Impulse i, Impulse j) => Show (FunctionNeuron i j) where
  show = show . typeOf

{-|
'Impulse's from 'FunctionNeuron', of type @j@.
-}
type FunctionFromImpulse i j = NeuronFromImpulse (FunctionNeuron i j)
-- | 'Impulse's for 'FunctionNeuron', of type @i@.
type FunctionForImpulse i j = NeuronForImpulse (FunctionNeuron i j)
{-|
Options for 'FunctionNeuron'. This option is defined:

[@function :: 'ImpulseTime' -> i -> \[j\]@] The function to apply to recieved 'Impulse's. Resulting 'Impulse's are send
in the list order. Default is to always return an empty list.
-}
type FunctionOptions i j = NeuronOptions (FunctionNeuron i j)

-- | A 'Neuron' which applies a given function to received 'Impulse's.
instance (Impulse i, Impulse j) => Neuron (FunctionNeuron i j) where
  type NeuronFromImpulse (FunctionNeuron i j) = j
  type NeuronForImpulse (FunctionNeuron i j) = i
  data NeuronOptions (FunctionNeuron i j) = FunctionOptions {
      function :: ImpulseTime -> i -> [j]
    }
  
  mkDefaultOptions = return FunctionOptions {
      function = defaultFunction
    }
  
  grow options = return $ FunctionNeuron options
  
  live nerve (FunctionNeuron FunctionOptions { function }) = forever $ do
    i <- head <$> waitAndSlurpForNeuron nerve -- just newest
    time <- getCurrentImpulseTime
    sendListFromNeuron nerve $ function time i
