{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, NamedFieldPuns #-}

{-|
This module defines a 'Neuron' which dumps all 'Impulse's it receives. You 'grow' it in 'Incubation' by using something like:

> nerveDump <- growNeuron (\o -> o { showInsteadOfDump = True }) :: NerveOnlyFor DumpNeuron

It is an example of a 'Neuron' which can recieve any 'Impulse' type.
-}

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
import Data.Data
import System.IO

import Control.Etage

data DumpNeuron = DumpNeuron DumpOptions deriving (Typeable)

-- | 'Impulse's from 'DumpNeuron'. This 'Neuron' does not define any 'Impulse's it would send.
type DumpFromImpulse = NeuronFromImpulse DumpNeuron
-- | 'Impulse's for 'DumpNeuron'. This 'Neuron' can recieve any 'Impulse' type.
type DumpForImpulse = NeuronForImpulse DumpNeuron
{-|
Options for 'DumpNeuron'. Those options are defined:

[@handle :: 'Handle'@] 'Handle' to which it dumps. Default is 'stdout'.

[@showInsteadOfDump :: 'Bool'@] Should it use 'show' when dumping 'Impulse's? By default it dumps 'impulseTime' and
'impulseValue' values.
-}
type DumpOptions = NeuronOptions DumpNeuron

-- | Impulse instance for 'DumpNeuron'.
instance Impulse DumpFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

-- | Impulse instance for 'DumpNeuron'.
instance Impulse DumpForImpulse where
  impulseTime (DumpForImpulse i) = impulseTime i
  impulseValue (DumpForImpulse i) = impulseValue i

deriving instance Show DumpFromImpulse

deriving instance Data DumpFromImpulse

instance Show DumpForImpulse where
  show (DumpForImpulse i) = show i

instance Eq DumpForImpulse where
  (==) = impulseEq

instance Ord DumpForImpulse where
  compare = impulseCompare

-- | A 'Neuron' which dumps all 'Impulse's it receives.
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
  
  live nerve (DumpNeuron DumpOptions { handle, showInsteadOfDump }) = forever $ do
    i <- getForNeuron nerve -- we want all not just newest
    if showInsteadOfDump
      then hPutStrLn handle $ show i
      else hPutStrLn handle $ show (impulseTime i) ++ ": " ++ show (impulseValue i)

instance Impulse i => ImpulseTranslator i DumpForImpulse where
  translate i = [DumpForImpulse i]
