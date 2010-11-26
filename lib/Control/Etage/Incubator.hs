{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Control.Etage.Incubator (
  incubate,
  growNeuron,
  attachTo,
  NerveBoth,
  NerveNone,
  NerveOnlyFrom,
  NerveOnlyFor,
  Incubation,
  -- * Internals
  growNerve
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans
import Data.List
import Data.Typeable
import System.IO

import Control.Etage.Chan
import Control.Etage.Propagate
import Control.Etage.Internals
import Control.Etage.Externals

data IncubationOperation a where
  NeuronOperation :: (Neuron n, GrowAxon (Axon (NeuronFromImpulse n) fromConductivity), GrowAxon (Axon (NeuronForImpulse n) forConductivity)) => (NeuronOptions n -> NeuronOptions n) -> IncubationOperation (Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity)
  AttachOperation :: forall from for forConductivity. (Typeable from, Typeable for, Typeable forConductivity) => Nerve from AxonConductive for forConductivity -> [Translatable from] -> IncubationOperation ()

type Incubation' a = ProgramT IncubationOperation IO a
newtype Incubation a = Incubation (Incubation' a) deriving (Monad, MonadIO, Applicative, Functor)

-- TODO: Check if all chans have been attached with type checking? (If this checking even shows as useful. And correct.)
incubate :: Incubation () -> IO ()
incubate (Incubation program) = mask $ \restore -> do
  (neurons, chans, attached) <- restore $ interpret [] [] [] program
  (flip finally) (detachManyAndWait neurons) $ do
    let na = nub chans \\ nub attached
        typ = unlines . map (\(ChanBox c) -> show $ neuronTypeOf c) $ na
    unless (null na) $ hPutStrLn stderr $ "Warning: It seems not all created nerves were attached. This causes a memory leak as produced data is not consumed. You should probably just define those nerves as NerveOnlyFor or NerveNone. Dangling nerves for neurons:\n" ++ typ
    restore waitForException

interpret :: [LiveNeuron] -> [ChanBox] -> [ChanBox] -> Incubation' () -> IO ([LiveNeuron], [ChanBox], [ChanBox])
interpret neurons chans attached = viewT >=> (eval neurons chans attached)
    where eval :: [LiveNeuron] -> [ChanBox] -> [ChanBox] -> ProgramViewT IncubationOperation IO () -> IO ([LiveNeuron], [ChanBox], [ChanBox])
          eval ns cs ats (Return _) = return (ns, cs, ats)
          eval ns cs ats (NeuronOperation optionsSetter :>>= is) = do
            nerve <- liftIO $ growNerve
            let c = getFromChan nerve
            bracketOnError (attach optionsSetter nerve) detach $ \n -> (interpret (n:ns) (c ++ cs) ats) . is $ nerve
          eval ns cs ats (AttachOperation from for :>>= is) = do
            let c = head . getFromChan $ from -- we know there exists from chan as type checking assures that (from is conductive)
            (from', ats') <- if c `notElem` ats
                               then return (from, c:ats)
                               else do
                                 dupFrom <- dupNerve from -- we have to duplicate from chan as it is attached multiple times
                                 return (dupFrom, ats) -- we store only original nerves in attached list
            propagate from' for
            (interpret ns cs ats') . is $ ()

growNeuron :: (Neuron n, GrowAxon (Axon (NeuronFromImpulse n) fromConductivity), GrowAxon (Axon (NeuronForImpulse n) forConductivity)) => (NeuronOptions n -> NeuronOptions n) -> Incubation (Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity)
growNeuron os = Incubation $ singleton (NeuronOperation os)

attachTo :: forall from for forConductivity. (Typeable from, Typeable for, Typeable forConductivity) => Nerve from AxonConductive for forConductivity -> [Translatable from] -> Incubation ()
attachTo n ts = Incubation $ singleton (AttachOperation n ts)

class GrowAxon a where
  growAxon :: IO a

instance Impulse i => GrowAxon (Axon i AxonConductive) where
  growAxon = Axon <$> newChan

instance GrowAxon (Axon i AxonNonConductive) where
  growAxon = return NoAxon

growNerve :: (Impulse from, Impulse for, GrowAxon (Axon from fromConductivity), GrowAxon (Axon for forConductivity)) => IO (Nerve from fromConductivity for forConductivity)
growNerve = do
  from <- growAxon
  for <- growAxon
  return $ Nerve from for

type NerveBoth n = Incubation (Nerve (NeuronFromImpulse n) AxonConductive (NeuronForImpulse n) AxonConductive)
type NerveNone n = Incubation (Nerve (NeuronFromImpulse n) AxonNonConductive (NeuronForImpulse n) AxonNonConductive)
type NerveOnlyFrom n = Incubation (Nerve (NeuronFromImpulse n) AxonConductive (NeuronForImpulse n) AxonNonConductive)
type NerveOnlyFor n = Incubation (Nerve (NeuronFromImpulse n) AxonNonConductive (NeuronForImpulse n) AxonConductive)

class (Typeable a, Eq a) => ChanClass a where
  neuronTypeOf :: a -> TypeRep

instance Impulse i => ChanClass (Chan i) where
  neuronTypeOf = head . typeRepArgs . head . typeRepArgs . typeOf -- we assume here that impulses are just NeuronFromImpulse or NeuronForImpulse

data ChanBox where
  ChanBox :: ChanClass a => a -> ChanBox

instance Eq ChanBox where
  ChanBox a == ChanBox b = typeOf a == typeOf b && cast a == Just b -- tests both typeOf and cast to be sure (cast could be defined to succeed for different types?)

getFromChan :: Nerve from fromConductivity for forConductivity -> [ChanBox]
getFromChan (Nerve (Axon c) _) = [ChanBox c]
getFromChan (Nerve NoAxon _) = []

dupNerve :: Nerve from AxonConductive for forConductivity -> IO (Nerve from AxonConductive for forConductivity)
dupNerve (Nerve (Axon c) for) = do
  c' <- dupChan c
  return $ Nerve (Axon c') for
