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
  -- >     nerveRandom `attachTo` [TranslatableFor nerveDump]
  incubate,
  growNeuron,
  attachTo,
  fuseWith,
  NerveBoth,
  NerveNone,
  NerveOnlyFrom,
  NerveOnlyFor,
  Incubation,
  -- * Internals
  -- | Be careful when using those functions as you have to assure your network is well-behaved:
  --
  -- * You should assure that for all 'Nerve's you defined as conductive from 'Neuron's and 'attach'ed them to 'Neuron's you
  -- really receive sent impulses, otherwise there will be a memory leak. You should probably just define those 'Nerve's you
  -- are not interested in 'Impulse's from as 'NerveOnlyFor' or 'NerveNone'.
  --
  -- * If you 'attach' multiple 'Neuron's to the same 'Nerve' you should probably take care of branching 'Nerve's correctly. For
  -- example, if multiple 'Neuron's are receiving from the same 'Nerve' you should first branch 'Nerve' with 'branchNerveFor',
  -- otherwise 'Neuron's will not receive all 'Impulse's as some other 'Neuron' will receive it first (but this can be also
  -- intentional).
  -- On the other hand, if you are receiving from the same 'Neuron' at multiple parts of the network you should branch
  -- 'Nerve' with 'branchNerveFrom' for each such part (or not, if intentional).
  --
  -- * This also holds for 'propagate': if you are using
  -- it multiple times with the same 'Nerve' as @from@ argument you should first branch it with 'branchNerveFrom'. (But it is
  -- probably easier to just use it once and list all @for@ 'Nerve's together.)
  --
  -- * And for 'fuse': all 'Nerve's you are 'fuse'-ing from should probably be first branched with 'branchNerveFrom' if you are
  -- also receiving from them somewhere else.
  --
  -- * Of course in a case of an exception or in general when your are doing cleanup you should assure that 'detach'
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
  fuse,
  branchNerveFor,
  branchNerveFrom,
  branchNerveBoth,
  cross
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
import Control.Etage.Fuse
import Control.Etage.Internals
import Control.Etage.Externals

data IncubationOperation a where
  NeuronOperation :: (Neuron n, GrowAxon (Axon (NeuronFromImpulse n) fromConductivity), GrowAxon (Axon (NeuronForImpulse n) forConductivity)) => (NeuronOptions n -> NeuronOptions n) -> IncubationOperation (Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity)
  AttachOperation :: forall from for forConductivity. (Impulse from, Impulse for) => Nerve from AxonConductive for forConductivity -> [TranslatableFor from] -> IncubationOperation ()
  FuseOperation :: forall i j. (Impulse i, Impulse j) => [TranslatableFrom i] -> (ImpulseTime -> [i] -> [j]) -> IncubationOperation (Nerve (FuseFromImpulse i j) AxonConductive (FuseForImpulse i j) AxonNonConductive)

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
        typ = unlines . map (\(ChanBox c) -> ' ' : show (impulseTypeOf c)) $ na
    unless (null na) $ hPutStrLn stderr $ "Warning: It seems not all created (conductive) nerves were attached. This causes a memory leak as send impulses are not received. You should probably just define those nerves as NerveOnlyFor or NerveNone. Dangling nerves have following impulse types in direction from a neuron:\n" ++ typ
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
            (from', ats') <- maybeBranch from ats
            propagate from' for
            interpret ns cs ats' . is $ ()
          eval ns cs ats (FuseOperation nerves fuser :>>= is) = do
            (nerves', ats') <- maybeBranchMany nerves ats
            n <- fuse nerves' fuser
            let c = getFromChan n
            interpret ns (c ++ cs) ats' . is $ n

maybeBranch :: forall from for forConductivity. (Impulse from, Impulse for) => Nerve from AxonConductive for forConductivity -> [ChanBox] -> IO (Nerve from AxonConductive for forConductivity, [ChanBox])
maybeBranch from ats = do
  let c = head . getFromChan $ from -- we know there exists from chan as type checking assures that (from is conductive)
  if c `notElem` ats
    then return (from, c:ats)
    else do
      branchFrom <- branchNerveFrom from -- we have to branch from chan as it is attached multiple times
      return (branchFrom, ats) -- we store only original nerves in attached list

maybeBranchMany :: forall i. (Impulse i) => [TranslatableFrom i] -> [ChanBox] -> IO ([TranslatableFrom i], [ChanBox])
maybeBranchMany [] ats = return ([], ats)
maybeBranchMany (TranslatableFrom n:ns) ats = do
  (n', ats') <- maybeBranch n ats
  (ns', ats'') <- maybeBranchMany ns ats'
  return (TranslatableFrom n':ns', ats'')

{-|
Grows a 'Neuron', taking a function which changes default options and returning a 'Nerve' 'attach'ed to the 'Neuron'.

Internally it combines 'growNerve' and 'attach'.
-}
growNeuron :: (Neuron n, GrowAxon (Axon (NeuronFromImpulse n) fromConductivity), GrowAxon (Axon (NeuronForImpulse n) forConductivity)) => (NeuronOptions n -> NeuronOptions n) -> Incubation (Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity)
growNeuron os = Incubation $ singleton (NeuronOperation os)

{-|
Attaches a 'Nerve' to other 'Nerve's so that 'Impulse's send from the 'Neuron' over the first 'Nerve' are received by 'Neuron's
of other 'Nerve's. 'Impulse's are 'propagate'd only in this direction, not in the other. If you want also the other direction use
'attachTo' again for that direction. 'attachTo' takes care of all the details (like branching 'Nerve's as necessary).

Internally it uses 'propagate'.
-}
attachTo :: forall from for forConductivity. (Impulse from, Impulse for) => Nerve from AxonConductive for forConductivity -> [TranslatableFor from] -> Incubation ()
attachTo _ [] = return ()
attachTo n ts = Incubation $ singleton (AttachOperation n ts)

{-|
Fuses 'Impulse's received from given 'Nerve's using the given function, sending them over the resulting 'grow'n 'Nerve'. 
'fuseWith' takes care of all the details (like branching 'Nerve's as necessary).

The important aspect of 'fuse'-ing is its synchronization behavior, as it requires exactly one 'Impulse' from each given 'Nerve' at
a time to 'fuse' them together. So it is important that all given 'Nerve's have more or less the equal density of 'Impulse's, otherwise
queues of some 'Nerve's will grow unproportionally because of the stalled 'Impulse's, causing at least a memory leak.

'impulseFuser' helper function can maybe help you with defining fusing function. 'fuseWith' uses type of the given function to construct
type of the resulting 'Nerve' so probably too polymorphic type will give you problems.

For example, 'fuse'-ing by 'sum'ing two 'Impulse's together can be achived like this:

> incubate $ do
>   nerveRandom1 <- (growNeuron :: NerveOnlyFrom (SequenceNeuron Int)) defaultOptions
>   nerveRandom2 <- (growNeuron :: NerveOnlyFrom (SequenceNeuron Int)) defaultOptions
>   nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) defaultOptions
>   
>   nerveFused <- [TranslatableFrom nerveRandom1, TranslatableFrom nerveRandom2] `fuseWith` (impulseFuser ((: []) . sum . concat))
>   
>   nerveFused `attachTo` [TranslatableFor nerveDump]

Internally it uses 'fuse'.
-}
fuseWith :: forall i j. (Impulse i, Impulse j) => [TranslatableFrom i] -> (ImpulseTime -> [i] -> [j]) -> Incubation (Nerve (FuseFromImpulse i j) AxonConductive (FuseForImpulse i j) AxonNonConductive)
fuseWith ts f = Incubation $ singleton (FuseOperation ts f)

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
  impulseTypeOf :: a -> TypeRep

instance Impulse i => ChanClass (Chan i) where
  impulseTypeOf = head . typeRepArgs . typeOf -- we remove Chan and leave just type of elements

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
