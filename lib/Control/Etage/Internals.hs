{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, NamedFieldPuns #-}
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
  waitForException
) where

import Control.Concurrent hiding (Chan)
import Data.Time.Clock.POSIX
import Data.Typeable
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
'Neuron's which operate on any 'Impulse' type. An example of such 'Neuron' is "Control.Etage.Function".
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
{-|
Is axon (one direction of a 'Nerve') conductive? No, it is not.

This is type checked and enforced. It is useful to specify nonconductive axons when you are not interested in 'Impulse's from a
particular axon (direction), making sure there will not be a memory leak because 'Impulse's would pile up.
-}
data AxonNonConductive deriving (Typeable)

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
An existentially quantified types encompassing all 'Nerve's which are conductive from a 'Neuron'.
-}
data FromNerve where
  FromNerve :: Impulse from => Nerve from AxonConductive for forConductivity -> FromNerve

{-|
An existentially quantified types encompassing all 'Nerve's which are conductive to a 'Neuron'.
-}
data ForNerve where
  ForNerve :: Impulse for => Nerve from fromConductivity for AxonConductive -> ForNerve

{-|
An existentially quantified types encompassing all 'Nerve's which are conductive in both directions.
-}
data BothNerve where
  BothNerve :: (Impulse from, Impulse for) => Nerve from AxonConductive for AxonConductive -> BothNerve

type NeuronDissolved = SampleVar ()
type NeuronId = ThreadId

deriving instance Typeable1 SampleVar

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
