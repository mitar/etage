{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Control.Etage.Incubator (
  -- * Incubation
  -- | 'Incubation' is a 'Monad' helping 'grow'ing a network of 'Neuron's and 'Nerve's while taking care of all the details and
  -- cleanup. It is the recommended and preferred way for 'grow'ing your networks.
  --
  -- A basic example of using 'Incubation' and of this data-flow framework would be a program where one 'Neuron' is generating
  -- 'Impulse's with random values ("Control.Etage.Sequence") and another 'Neuron' printing them out ("Control.Etage.Dump"):
  --
  -- > main = do
  -- >   prepareEnvironment
  -- >   
  -- >   incubate $ do
  -- >     nerveRandom <- (growNeuron :: NerveOnlyFrom (SequenceNeuron Int)) defaultOptions
  -- >     nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) defaultOptions
  -- >     
  -- >     nerveRandom `attachTo` [Translatable nerveDump]
  incubate,
  growNeuron,
  attachTo,
  NerveBoth,
  NerveNone,
  NerveOnlyFrom,
  NerveOnlyFor,
  Incubation,
  Translatable(..),
  -- * Internals
  -- | Be careful when using those functions as you have to assure your network is well-behaved:
  --
  -- * You should assure that for all 'Nerve's you defined as conductive from 'Neuron's and 'attach'ed them to 'Neuron's you
  -- really receive sent impulses, otherwise there will be a memory leak. You should probably just define those nerves
  -- as 'NerveOnlyFor' or 'NerveNone'.
  --
  -- * If you 'attach' multiple 'Neuron's to the same 'Nerve' you should probably take care of branching 'Nerve's correctly. For
  -- example, if multiple 'Neuron's are receiving from the same 'Nerve' you should first branch 'Nerve' with 'branchNerveFor',
  -- otherwise 'Neuron's will not receive all 'Impulse's as some other 'Neuron' will receive it first (but this can be also
  -- intentional).
  -- On the other hand, if you are receiving from the same 'Neuron' at multiple parts of the network you should branch
  -- 'Nerve' with 'branchNerveFrom' for each such part (or not, if intentional). This also holds for 'propagate': if you are using
  -- it multiple times with the same 'Nerve' as @from@ argument you should first branch it with 'branchNerveFrom'. (But it is
  -- probably easier to just use it once and list all @for@ 'Nerve's together.)
  --
  -- * And of course in a case of an exception or in general when your are doing cleanup you should assure that 'detach'
  -- (or 'detachAndWait') is called for each 'LiveNeuron' (or 'detachMany' or 'detachManyAndWait').
  --
  -- They are exposed so that you can decouple 'grow'ing and 'dissolve'-ing handling and that you can attach 'Nerve's
  -- in some special ways. If you do not need that use 'Incubation'.
  --
  -- For example, your 'Neuron' can 'grow' and use another 'Neuron' (in this example "Control.Etage.Worker") like this:
  --
  -- > data YourNeuron = YourNeuron ... LiveNeuron (Nerve WorkerFromImpulse AxonNonConductive WorkerForImpulse AxonConductive) deriving (Typeable)
  -- >
  -- > grow options = do
  -- >   ...
  -- >   nerve <- growNerve
  -- >   bracketOnError (attach defaultOptions nerve) detachAndWait $ \neuron -> do
  -- >     ...
  -- >     return $ YourNeuron ... neuron nerve
  -- >
  -- > dissolve (YourNeuron ... neuron _) = do
  -- >   detachAndWait neuron
  -- >   ...
  --
  -- We use 'bracketOnError' there to be sure that 'Neuron' is properly 'dissolve'd even if there is an exception later on in
  -- 'grow'ing the parent 'Neuron'. And we use 'detachAndWait' so that we give time for child 'Neuron' to 'dissolve' properly.
  -- Which 'Neuron' you want is in this case inferred from the type of the 'Nerve' you defined.
  growNerve,
  propagate,
  branchNerveFor,
  branchNerveFrom,
  branchNerveBoth
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
{-|
An 'Incubation' monad type. It makes sure network is 'grow'n properly and that everything is cleaned up as necessary.
-}
newtype Incubation a = Incubation (Incubation' a) deriving (Monad, MonadIO, Applicative, Functor)

-- TODO: Check if all chans have been attached with type checking (type nats)? (If this checking even shows as useful. And correct.)
{-|
Runs an 'Incubation', 'grow'ing 'Neuron's and 'attach'ing 'Nerve's and after that waiting for them to finish and cleanup.
It rethrows any exception which might have been thrown.
-}
incubate :: Incubation () -> IO ()
incubate (Incubation program) = mask $ \restore -> do
  (neurons, chans, attached) <- restore $ interpret [] [] [] program
  flip finally (detachManyAndWait neurons) $ do
    let na = nub chans \\ nub attached
        typ = unlines . map (\(ChanBox c) -> ' ' : show (neuronTypeOf c)) $ na
    unless (null na) $ hPutStrLn stderr $ "Warning: It seems not all created nerves were attached. This causes a memory leak as send impulses are not received. You should probably just define those nerves as NerveOnlyFor or NerveNone. Dangling nerves for neurons:\n" ++ typ
    restore waitForException

interpret :: [LiveNeuron] -> [ChanBox] -> [ChanBox] -> Incubation' () -> IO ([LiveNeuron], [ChanBox], [ChanBox])
interpret neurons chans attached = viewT >=> eval neurons chans attached
    where eval :: [LiveNeuron] -> [ChanBox] -> [ChanBox] -> ProgramViewT IncubationOperation IO () -> IO ([LiveNeuron], [ChanBox], [ChanBox])
          eval ns cs ats (Return ()) = return (ns, cs, ats)
          eval ns cs ats (NeuronOperation optionsSetter :>>= is) = do
            nerve <- liftIO growNerve
            let c = getFromChan nerve
            bracketOnError (attach optionsSetter nerve) detachAndWait $ \n -> interpret (n:ns) (c ++ cs) ats . is $ nerve
          eval ns cs ats (AttachOperation from for :>>= is) = do
            let c = head . getFromChan $ from -- we know there exists from chan as type checking assures that (from is conductive)
            (from', ats') <- if c `notElem` ats
                               then return (from, c:ats)
                               else do
                                 branchFrom <- branchNerveFrom from -- we have to branch from chan as it is attached multiple times
                                 return (branchFrom, ats) -- we store only original nerves in attached list
            propagate from' for
            interpret ns cs ats' . is $ ()

{-|
Grows a 'Neuron', taking a function which changes default options and returning a 'Nerve' 'attach'ed to the 'Neuron'.

Internally it combines 'growNerve' and 'attach'.
-}
growNeuron :: (Neuron n, GrowAxon (Axon (NeuronFromImpulse n) fromConductivity), GrowAxon (Axon (NeuronForImpulse n) forConductivity)) => (NeuronOptions n -> NeuronOptions n) -> Incubation (Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity)
growNeuron os = Incubation $ singleton (NeuronOperation os)

{-|
Attaches a 'Nerve' to other 'Nerve's so that 'Impulse's send from the 'Neuron' over the first 'Nerve' are received by 'Neuron's
of other 'Nerve's. 'Impulse's are 'propagate'd only in this direction, not in the other. If you want also the other direction use
'attachTo' again for that direction.

Internally it uses 'propagate'.
-}
attachTo :: forall from for forConductivity. (Typeable from, Typeable for, Typeable forConductivity) => Nerve from AxonConductive for forConductivity -> [Translatable from] -> Incubation ()
attachTo n ts = Incubation $ singleton (AttachOperation n ts)

class GrowAxon a where
  growAxon :: IO a

instance Impulse i => GrowAxon (Axon i AxonConductive) where
  growAxon = Axon <$> newChan

instance GrowAxon (Axon i AxonNonConductive) where
  growAxon = return NoAxon

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
Type which helps you define (fix) a type of the 'growNeuron' function so that compiler knows whith 'Neuron' instance to choose.
It takes type of the 'Neuron' you want to 'grow' as an argument and specifies a 'Nerve' which is conductive in both directions.
-}
type NerveBoth n = (NeuronOptions n -> NeuronOptions n) -> Incubation (Nerve (NeuronFromImpulse n) AxonConductive (NeuronForImpulse n) AxonConductive)
{-|
Type which helps you define (fix) a type of the 'growNeuron' function so that compiler knows whith 'Neuron' instance to choose.
It takes type of the 'Neuron' you want to 'grow' as an argument and specifies a 'Nerve' which is not conductive in any directions.
-}
type NerveNone n = (NeuronOptions n -> NeuronOptions n) -> Incubation (Nerve (NeuronFromImpulse n) AxonNonConductive (NeuronForImpulse n) AxonNonConductive)
{-|
Type which helps you define (fix) a type of the 'growNeuron' function so that compiler knows whith 'Neuron' instance to choose.
It takes type of the 'Neuron' you want to 'grow' as an argument and specifies a 'Nerve' which is conductive only in the direction from the 'Neuron'.
-}
type NerveOnlyFrom n = (NeuronOptions n -> NeuronOptions n) -> Incubation (Nerve (NeuronFromImpulse n) AxonConductive (NeuronForImpulse n) AxonNonConductive)
{-|
Type which helps you define (fix) a type of the 'growNeuron' function so that compiler knows whith 'Neuron' instance to choose.
It takes type of the 'Neuron' you want to 'grow' as an argument and specifies a 'Nerve' which is conductive only in the direction to the 'Neuron'.
-}
type NerveOnlyFor n = (NeuronOptions n -> NeuronOptions n) -> Incubation (Nerve (NeuronFromImpulse n) AxonNonConductive (NeuronForImpulse n) AxonConductive)

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

{-|
Branches 'Nerve' on the 'Neuron' side. This allows multiple 'Neuron's to be attached to it and still receive all 'Impulse's
(otherwise just the first 'Neuron' which would read from a 'Nerve' would receive a given 'Impulse').
Only new 'Impulse's from a moment of branching on are conducted over new the branch, old 'Impulse's are not reconducted.
Branching can be applied multiple times.
-}
branchNerveFor :: Nerve from fromConductivity for AxonConductive -> IO (Nerve from fromConductivity for AxonConductive)
branchNerveFor (Nerve from (Axon c)) = do
  c' <- dupChan c
  return $ Nerve from (Axon c')

{-|
Branches 'Nerve' on the other (non-'Neuron') side. This allows using the same 'Nerve' at multiple parts of the network (program)
and still receive all 'Impulse's from 'Neuron' at all parts of the network (otherwise just the first read from a 'Nerve' would
receive a given 'Impulse').
Only new 'Impulse's from a moment of branching on are conducted over the new branch, old 'Impulse's are not reconducted.
Branching can be applied multiple times.
-}
branchNerveFrom :: Nerve from AxonConductive for forConductivity -> IO (Nerve from AxonConductive for forConductivity)
branchNerveFrom (Nerve (Axon c) for) = do
  c' <- dupChan c
  return $ Nerve (Axon c') for

{-|
Branches 'Nerve' on both sides. Same as both 'branchNerveFor' and 'branchNerveFrom'.
-}
branchNerveBoth :: Nerve from AxonConductive for AxonConductive -> IO (Nerve from AxonConductive for AxonConductive)
branchNerveBoth = branchNerveFrom >=> branchNerveFor
