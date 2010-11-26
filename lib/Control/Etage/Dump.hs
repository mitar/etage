{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Dump (
  DumpNeuron,
  DumpFromImpulse,
  DumpForImpulse,
  DumpOptions,
  NeuronFromImpulse,
  NeuronForImpulse(..),
  NeuronOptions(..)
) where

import Control.Monad
import Data.Typeable
import System.IO

import Control.Etage

data DumpNeuron = DumpNeuron DumpOptions deriving (Typeable)

type DumpFromImpulse = NeuronFromImpulse DumpNeuron
type DumpForImpulse = NeuronForImpulse DumpNeuron
type DumpOptions = NeuronOptions DumpNeuron

instance Impulse DumpFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

instance Impulse DumpForImpulse where
  impulseTime (DumpForImpulse i) = impulseTime i
  impulseValue (DumpForImpulse i) = impulseValue i

deriving instance Show DumpFromImpulse

instance Show DumpForImpulse where
  show (DumpForImpulse i) = show i

instance Eq DumpForImpulse where
  (==) = impulseEq

instance Ord DumpForImpulse where
  compare = impulseCompare

instance Neuron DumpNeuron where
  data NeuronFromImpulse DumpNeuron
  data NeuronForImpulse DumpNeuron where
    DumpForImpulse :: Impulse i => i -> DumpForImpulse
  data NeuronOptions DumpNeuron = DumpOptions {
      handle :: Handle,
      showInsteadOfDump :: Bool
    } deriving (Eq, Show)
  
  mkDefaultOptions = return DumpOptions {
      handle = stdout,
      showInsteadOfDump = False
    }
  
  grow options = return $ DumpNeuron options
  
  live nerve (DumpNeuron DumpOptions { .. }) = forever $ do
    i <- getForNeuron nerve
    if showInsteadOfDump
      then hPutStrLn handle $ show i
      else hPutStrLn handle $ show (impulseTime i) ++ ": " ++ show (impulseValue i)

instance Impulse i => ImpulseTranslator i DumpForImpulse where
  translate i = [DumpForImpulse i]
