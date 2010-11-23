{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}

module Control.Etage.Dump where

import Control.Monad
import System.IO

import Control.Etage.Types

data DumpNeuron = DumpNeuron DumpOptions

instance Impulse DumpForImpulse where
  impulseTime (DumpForImpulse i) = impulseTime i
  impulseValue (DumpForImpulse i) = impulseValue i

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance Impulse DumpFromImpulse where
  impulseTime _ = undefined
  impulseValue _ = undefined

type LiveDumpNeuron = LiveNeuron DumpNeuron
type DumpForImpulse = NeuronForImpulse DumpNeuron
type DumpFromImpulse = NeuronFromImpulse DumpNeuron
type DumpOptions = NeuronOptions DumpNeuron

instance Show DumpForImpulse where
  show (DumpForImpulse i) = show i

instance Eq DumpForImpulse where
  (==) = impulseEq

instance Ord DumpForImpulse where
  compare = impulseCompare

-- TODO: Remove in favor of automatic deriving in GHC 7.0?
instance Show DumpFromImpulse where
  show _ = undefined

instance Neuron DumpNeuron where
  data LiveNeuron DumpNeuron = LiveDumpNeuron NeuronDissolved NeuronId
  data NeuronForImpulse DumpNeuron where
    DumpForImpulse :: Impulse i => i -> DumpForImpulse
  data NeuronFromImpulse DumpNeuron
  data NeuronOptions DumpNeuron = DumpOptions {
      handle :: Handle,
      showInsteadOfDump :: Bool
    } deriving (Eq, Show)
  
  mkLiveNeuron = LiveDumpNeuron
  getNeuronDissolved (LiveDumpNeuron dissolved _) = dissolved
  getNeuronId (LiveDumpNeuron _ nid) = nid
  
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
