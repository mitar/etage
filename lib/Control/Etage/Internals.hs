{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, NamedFieldPuns, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Etage.Internals (
  Axon(..),
  Nerve(..),
  Impulse(..),
  LiveNeuron(..),
  ImpulseValue,
  ImpulseTime,
  AxonConductive,
  AxonNonConductive,
  FromNerve(..),
  ForNerve(..),
  BothNerve(..),
  NeuronDissolved,
  NeuronId,
  waitForException,
  GrowAxon(..),
  growNerve,
  cross
) where

import Control.Applicative
import Control.Concurrent hiding (Chan, newChan)
import Data.Data
import Data.Time.Clock.POSIX
import Numeric
import Text.ParserCombinators.ReadP

import Control.Etage.Chan

-- TODO: Find better general representation for values (something analog to what a hologram is, so that it can be gradually simplified and gradually reconstructed). Could be an Incubation program itself?
-- | Type of a general representation of 'Impulse' values (data payload). Currently it is just a list of 'Rational' values.
type ImpulseValue = [Rational]

-- | Type of 'Impulse' timestamp. You can use 'getCurrentImpulseTime' for timestamp representing current time.
type ImpulseTime = POSIXTime

instance Read ImpulseTime where
  readsPrec _ r = do
    (time, sec) <- readFloat r
    ('s', rest) <- readP_to_S (char 's') sec
    return (time, rest)

{-|
Type class with common methods for impulses send over 'Nerve's and processed in 'Neuron's so that it is possible to define
'Neuron's which operate on any 'Impulse' type by using 'AnyImpulse' type as their receiving 'Impulse's type. An example of
such 'Neuron' is "Control.Etage.Dump".
-}
class (Show i, Typeable i) => Impulse i where
  -- | This method should return a timestamp when the 'Impulse' was created/finalized what should be the moment just before it is send
  -- over the 'Nerve', the moment it formed into its final form and started leaving the 'Neuron'. As Haskell is a lazy language this
  -- does not mean that at that moment all values the 'Impulse' defines are really already evaluated (they are evaluated when they are
  -- needed, probably in some other 'Neuron').
  --
  -- You can do something like:
  --
  -- > time <- getCurrentImpulseTime
  -- > sendFromNeuron nerve YourImpulse { impulseTimestamp = time, ... }
  impulseTime :: i -> ImpulseTime
  -- | This method should return all values (data payload) the 'Impulse' defines. Currently order and format is not yet finalized so
  -- it is just a list of 'Rational' values in some order (for now it probably should be the order in which the values are defined
  -- in the 'Impulse' constructor).
  --
  -- It is meant to allow general 'Neurons' which can work on any 'Impulse' type to be developed. For example 'Neuron's which
  -- implement some machine learning or data mining algorithms. It is on purpose that values are cleared of any semantic
  -- meaning so algorithms have better chance not to get in touch with some unintended domain specific knowledge.
  impulseValue :: i -> ImpulseValue

{-|
Is axon (one direction of a 'Nerve') conductive? Yes, it is.

This is type checked and enforced. If you define axon as conductive you have to make make sure that 'Impulse's send along it are
really read somewhere, otherwise a memory leak will occur.
-}
data AxonConductive deriving (Typeable)

deriving instance Data AxonConductive

{-|
Is axon (one direction of a 'Nerve') conductive? No, it is not.

This is type checked and enforced. It is useful to specify nonconductive axons when you are not interested in 'Impulse's from a
particular axon (direction), making sure there will not be a memory leak because 'Impulse's would pile up.
-}
data AxonNonConductive deriving (Typeable)

deriving instance Data AxonNonConductive

data Axon impulse conductivity where
  Axon :: Impulse i => Chan i -> Axon i AxonConductive
  NoAxon :: Axon i AxonNonConductive

{-|
Type representing a 'Nerve' between 'Neuron's. It is bi-directional (from and to a 'Neuron', each direction being one axon) and you
can specify type of 'Impulse's traveling along the axon and its conductivity (with AxonConductive or
AxonNonConductive).

You mostly do not need to specify this type manually if you are using 'growNeuron' and one of 'NerveBoth', 'NerveNone',
'NerveOnlyFrom' and 'NerveOnlyFor' types.
-}
data Nerve from fromConductivity for forConductivity where
  Nerve :: (Impulse from, Impulse for) => Axon from fromConductivity -> Axon for forConductivity -> Nerve from fromConductivity for forConductivity

deriving instance Typeable4 Nerve

instance (Typeable forConductivity, Typeable fromConductivity, Typeable from, Typeable for) => Show (Nerve from fromConductivity for forConductivity) where
  show = show . typeOf

{-|
An existentially quantified type encompassing all 'Nerve's which are conductive from a 'Neuron'.
-}
data FromNerve where
  FromNerve :: Impulse from => Nerve from AxonConductive for forConductivity -> FromNerve

{-|
An existentially quantified type encompassing all 'Nerve's which are conductive to a 'Neuron'.
-}
data ForNerve where
  ForNerve :: Impulse for => Nerve from fromConductivity for AxonConductive -> ForNerve

{-|
An existentially quantified type encompassing all 'Nerve's which are conductive in both directions.
-}
data BothNerve where
  BothNerve :: (Impulse from, Impulse for) => Nerve from AxonConductive for AxonConductive -> BothNerve

type NeuronDissolved = SampleVar ()
type NeuronId = ThreadId

#if !(MIN_VERSION_base(4,5,0))
deriving instance Typeable1 SampleVar
#endif

instance Show NeuronDissolved where
  show = show . typeOf

{-|
Type representing a 'live' 'Neuron'.
-}
data LiveNeuron = LiveNeuron NeuronDissolved NeuronId deriving (Eq, Typeable)

instance Show LiveNeuron where
  show = show . typeOf

waitForException :: IO a
waitForException = newEmptyMVar >>= takeMVar

class GrowAxon a where
  growAxon :: IO a

instance Impulse i => GrowAxon (Axon i AxonConductive) where
  growAxon = Axon <$> newChan

instance GrowAxon (Axon i AxonNonConductive) where
  growAxon = return NoAxon

-- TODO: Make an incubation version of growNerve which would follow if it was correctly attached (but how to follow if it is used as an option to a neuron and is consumed there?)
{-|
Grows an unattached 'Nerve'. By specifying type of the 'Nerve' you can specify conductivity of both directions (which is then
type checked for consistency around the program) and thus specify which 'Impulse's you are interested in (and thus limit possible
memory leak). With type of 'Impulse's this 'Nerve' is capable of conducting you can also specify which 'Neuron' you are interested
in 'grow'ing on the one end of the 'Nerve'.

For example, you could grow a 'Nerve' for "Control.Etage.Sequence" 'Neuron' and 'Neuron' itself like this:

> nerve <- growNerve :: IO (Nerve (SequenceFromImpulse Int) AxonConductive (SequenceForImpulse Int) AxonNonConductive)
> neuron <- attach defaultOptions nerve

and for example print all 'Impulse's as they are coming in:

> print =<< getContentsFromNeuron nerve

Check 'growNeuron' for a more high-level function (of 'Incubation') which both 'grow's a 'Neuron' and corresponding 'Nerve' taking
care of all the details. Use this function only if you need decoupled 'grow'ing.
-}
growNerve :: (Impulse from, Impulse for, GrowAxon (Axon from fromConductivity), GrowAxon (Axon for forConductivity)) => IO (Nerve from fromConductivity for forConductivity)
growNerve = do
  from <- growAxon
  for <- growAxon
  return $ Nerve from for

{-|
Crosses axons around in a 'Nerve'. Useful probably only when you want to 'attachTo' 'Nerve' so that it looks as 'Impulse's are comming
from a 'Neuron' and are not send to a 'Neuron'. So in this case you are 'attach'ing 'Nerve' in a direction away from a 'Neuron' and not
towards it, what is a default.

For example, you can do something like this:

> nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) defaultOptions
> nerveOnes <- (growNeuron :: NerveOnlyFrom (SequenceNeuron Int)) (\o -> o { valueSource = repeat 1 })
> nerveTwos <- (growNeuron :: NerveOnlyFrom (SequenceNeuron Int)) (\o -> o { valueSource = repeat 2 })
> 
> nerveOnes `attachTo` [TranslatableFor (cross nerveTwos)]
> nerveTwos `attachTo` [TranslatableFor nerveDump]

Of course in this example you could simply 'attachTo' both 'Nerve's to "Control.Etage.Dump" 'Neuron'. So 'cross' is probably useful
only when using 'Nerve's unattached to its 'Neuron' (made by 'growNerve', for example) and/or when using such 'Nerve's with
'Neuron's which operate on how 'Impulse's are 'propagate'd (or 'fuse'd).
-}
cross :: Nerve from fromConductivity for forConductivity -> Nerve for forConductivity from fromConductivity
cross (Nerve from for) = Nerve for from
