{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, DeriveDataTypeable, NamedFieldPuns #-}

{-|
This module defines a 'Neuron' which dumps all 'Impulse's it receives. You 'grow' it in 'Incubation' by using something like:

> nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) (\o -> o { showInsteadOfDump = True })

It is an example of a 'Neuron' which can recieve any 'Impulse' type. It processes all 'Impulse's it receives.
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

defaultPrefix :: String
defaultPrefix = ""

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

[@prefix :: 'String'@] Prefix to use when dumping. Default is no prefix.
-}
type DumpOptions = NeuronOptions DumpNeuron

-- | A 'Neuron' which dumps all 'Impulse's it receives.
instance Neuron DumpNeuron where
  type NeuronFromImpulse DumpNeuron = NoImpulse
  type NeuronForImpulse DumpNeuron = AnyImpulse
  data NeuronOptions DumpNeuron = DumpOptions {
      handle :: Handle,
      showInsteadOfDump :: Bool,
      prefix :: String
    } deriving (Eq, Show)
  
  mkDefaultOptions = return DumpOptions {
      handle = stdout,
      showInsteadOfDump = False,
      prefix = defaultPrefix
    }
  
  grow options = return $ DumpNeuron options
  
  live nerve (DumpNeuron DumpOptions { handle, showInsteadOfDump, prefix }) = forever $ do
    i <- getForNeuron nerve -- we want all not just newest
    if showInsteadOfDump
      then hPutStrLn handle $ prefix ++ show i
      else hPutStrLn handle $ prefix ++ show (impulseTime i) ++ ": " ++ show (impulseValue i)
