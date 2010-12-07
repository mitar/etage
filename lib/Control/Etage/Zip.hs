{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances, NamedFieldPuns #-}

{-|
This module defines a 'Neuron' which combines 'Impulse's from multiple given 'Nerve's using a given function.
You 'grow' it in 'Incubation' by using something like:

> nerveZip <- growNeuron (\o -> o { nerves = [FromNerve nerveOnes, FromNerve nerveTwos] }) :: NerveOnlyFrom ZipNeuron

It combines 'Impulse's synchronously, that is, each time it takes from each 'Nerve' exactly one 'Impulse' (in list order, waiting until
it can do so), not skipping any. It is an example of a 'Neuron' which takes 'Nerve's as an option.

An example network which is generating a Fibonacci sequence:

> nerveDump <- growNeuron defaultOptions :: NerveOnlyFor DumpNeuron
> nerveFunction <- growNeuron defaultOptions :: NerveBoth FunctionNeuron
> 
> nervePrev <- liftIO $ growNerve :: NerveOnlyFrom FunctionNeuron
> nerveFunction' <- liftIO $ branchNerveFrom nerveFunction
> nerveZip <- growNeuron (\o -> o { nerves = [FromNerve nervePrev, FromNerve nerveFunction'] }) :: NerveOnlyFrom ZipNeuron
> 
> nerveZip `attachTo` [Translatable nerveFunction]
> nerveFunction `attachTo` [Translatable nerveDump, Translatable (cross nervePrev)]
> 
> liftIO $ do
>   time <- getCurrentImpulseTime
>   sendFromNeuron nervePrev Value { impulseTimestamp = time, value = 0 }
>   sendFromNeuron nerveFunction' Value { impulseTimestamp = time, value = 1 }
-}

module Control.Etage.Zip (
  ZipNeuron,
  ZipFromImpulse,
  ZipForImpulse,
  ZipOptions,
  NeuronFromImpulse,
  NeuronForImpulse,
  NeuronOptions(..)
) where

import Control.Applicative
import Control.Monad
import Data.Typeable

import Control.Etage

-- TODO: Make it an internal neuron and a public function similar to propagate

defaultZipper :: [ImpulseValue] -> ImpulseValue
defaultZipper = concat

data ZipNeuron = ZipNeuron ZipOptions deriving (Typeable)

instance Show ZipNeuron where
  show = show . typeOf

{-|
'Impulse's from 'ZipNeuron'. This 'Impulse' constructor is defined:

[@Value { impulseTimestamp :: 'ImpulseTime', zipped :: 'ImpulseValue' }@]
@impulseTimestamp@ is time when the impulses was combined, @zipped@ contains the combined impulses.
-}
type ZipFromImpulse = NeuronFromImpulse ZipNeuron
-- | 'Impulse's for 'ZipNeuron'. This 'Neuron' does not define any 'Impulse's it would receive.
type ZipForImpulse = NeuronForImpulse ZipNeuron
{-|
Options for 'ZipNeuron'. Those options are defined:

[@zipper :: \['ImpulseValue'\] -> 'ImpulseValue'@] The function to combine 'Impulse's. Default is 'concat'.

[@nerves :: \['FromNerve'\]@] The list of 'Nerve's to combine 'Impulse's from.
-}
type ZipOptions = NeuronOptions ZipNeuron

-- | Impulse instance for 'ZipNeuron'.
instance Impulse ZipFromImpulse where
  impulseTime Zipped { impulseTimestamp } = impulseTimestamp
  impulseValue Zipped { zipped } = zipped

-- | Impulse instance for 'ZipNeuron'.
instance Impulse ZipForImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

deriving instance Show ZipForImpulse

-- | A 'Neuron' which combines 'Impulse's from multiple 'Nerve's.
instance Neuron ZipNeuron where
  data NeuronFromImpulse ZipNeuron = Zipped {
      impulseTimestamp :: ImpulseTime, -- time is first so that ordering is first by time
      zipped :: ImpulseValue
    } deriving (Eq, Ord, Read, Show)
  data NeuronForImpulse ZipNeuron
  data NeuronOptions ZipNeuron = ZipOptions {
      zipper :: [ImpulseValue] -> ImpulseValue,
      nerves :: [FromNerve]
    }
  
  mkDefaultOptions = return ZipOptions {
      zipper = defaultZipper,
      nerves = undefined
    }
  
  grow options = return $ ZipNeuron options
  
  live nerve (ZipNeuron ZipOptions { zipper, nerves }) = forever $ do
    z <- zipper <$> mapM (\(FromNerve n) -> impulseValue <$> getFromNeuron n) nerves
    time <- getCurrentImpulseTime
    sendFromNeuron nerve Zipped { impulseTimestamp = time, zipped = z }
