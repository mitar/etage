{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, DeriveDataTypeable, NamedFieldPuns #-}

{-|
This module defines a 'Neuron' which dumps all 'Impulse's it receives. You 'grow' it in 'Incubation' by using something like:

> nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) (\o -> o { showInsteadOfDump = True })

It is an example of a 'Neuron' which can recieve any 'Impulse' type.
-}

module Control.Etage.Dump (
  DumpNeuron,
  DumpFromImpulse,
  DumpForImpulse,
  DumpOptions,
  NeuronOptions(..)
) where

import Control.Monad
import Data.Data
import System.IO

import Control.Etage

data DumpNeuron = DumpNeuron DumpOptions deriving (Typeable)

-- | 'Impulse's from 'DumpNeuron'. This 'Neuron' does not define any 'Impulse's it would send, 'NoImpulse'.
type DumpFromImpulse = NeuronFromImpulse DumpNeuron
-- | 'Impulse's for 'DumpNeuron'. This 'Neuron' can recieve any 'Impulse' type, 'AnyImpulse'.
type DumpForImpulse = NeuronForImpulse DumpNeuron
{-|
Options for 'DumpNeuron'. Those options are defined:

[@handle :: 'Handle'@] 'Handle' to which it dumps. Default is 'stdout'.

[@showInsteadOfDump :: 'Bool'@] Should it use 'show' when dumping 'Impulse's? By default it dumps 'impulseTime' and
'impulseValue' values.
-}
type DumpOptions = NeuronOptions DumpNeuron

-- | A 'Neuron' which dumps all 'Impulse's it receives.
instance Neuron DumpNeuron where
  type NeuronFromImpulse DumpNeuron = NoImpulse
  type NeuronForImpulse DumpNeuron = AnyImpulse
  data NeuronOptions DumpNeuron = DumpOptions {
      handle :: Handle,
      showInsteadOfDump :: Bool
    } deriving (Eq, Show)
  
  mkDefaultOptions = return DumpOptions {
      handle = stdout,
      showInsteadOfDump = False
    }
  
  grow options = return $ DumpNeuron options
  
  live nerve (DumpNeuron DumpOptions { handle, showInsteadOfDump }) = forever $ do
    i <- getForNeuron nerve -- we want all not just newest
    if showInsteadOfDump
      then hPutStrLn handle $ show i
      else hPutStrLn handle $ show (impulseTime i) ++ ": " ++ show (impulseValue i)
