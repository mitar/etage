{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Internals (
  Axon(..),
  Nerve(..),
  Impulse(..),
  ImpulseValue,
  ImpulseTime,
  AxonConductive,
  AxonNonConductive
) where

import Data.Time.Clock.POSIX
import Data.Typeable
import Numeric
import Text.ParserCombinators.ReadP

import Control.Etage.Chan

-- TODO: Find better general representation for values (something analog to what a hologram is, so that it can be gradually simplified and gradually reconstructed)
type ImpulseValue = [Rational]

type ImpulseTime = POSIXTime

instance Read ImpulseTime where
  readsPrec _ r = do
    (time, sec) <- readFloat r
    ('s', rest) <- readP_to_S (char 's') sec
    return (time, rest)

{-|
Type class with common operations for impulses send over 'Nerve's and processed in 'Neuron's.
-}
class (Show i, Typeable i) => Impulse i where
  impulseTime :: i -> ImpulseTime
  impulseValue :: i -> ImpulseValue

data AxonConductive deriving Typeable
data AxonNonConductive deriving Typeable

data Axon impulse conductivity where
  Axon :: Impulse i => Chan i -> Axon i AxonConductive
  NoAxon :: Axon i AxonNonConductive

data Nerve from fromConductivity for forConductivity where
  Nerve :: (Impulse from, Impulse for) => Axon from fromConductivity -> Axon for forConductivity -> Nerve from fromConductivity for forConductivity
