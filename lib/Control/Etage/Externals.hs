{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, NamedFieldPuns #-}

module Control.Etage.Externals (
  -- * 'Neuron's and 'Impulse's
  -- | Using only built-in 'Neuron's is not much fun. Main idea of this data-flow framework is to ease development of your own
  -- 'Neuron's (data processing units).
  Neuron(..),
  attach',
  detach,
  detachAndWait,
  detachMany,
  detachManyAndWait,
  LiveNeuron,
  DissolveException,
  dissolving,
  DissolvingException,

  mkNeuronMapOnRandomCapability,
  NeuronMapCapability(..),

  defaultOptions,
  
  Impulse(..),
  ImpulseTime,
  ImpulseValue,
  ImpulseTranslator(..),

  translateAndSend,

  Nerve,
  AxonConductive,
  AxonNonConductive,

  -- * Sending and receiving outside the 'Neuron'
  -- | Those functions are used outside the 'Neuron' when interacting with it.
  sendForNeuron,
  getFromNeuron,
  maybeGetFromNeuron,
  slurpFromNeuron,
  waitAndSlurpFromNeuron,
  getContentsFromNeuron,
  sendListForNeuron,

  -- * Sending and receiving inside the 'Neuron'
  -- | Those functions are used inside the 'Neuron' when implementing it.
  sendFromNeuron,
  getForNeuron,
  maybeGetForNeuron,
  slurpForNeuron,
  waitAndSlurpForNeuron,
  getNewestForNeuron,
  getContentsForNeuron,
  sendListFromNeuron,

  -- * Helper functions
  prepareEnvironment,
  getCurrentImpulseTime,
  impulseEq,
  impulseCompare
) where

import Prelude hiding (catch)

import Control.Concurrent hiding (Chan, writeChan, readChan, isEmptyChan, getChanContents)
import Data.Data
import Data.Function
import Data.List
import Control.Exception
import Data.Time.Clock.POSIX
import GHC.IO (unsafeUnmask)
import GHC.Conc (forkOnIO, numCapabilities)
import System.IO
import System.Posix.Signals
import System.Random

import Control.Etage.Chan
import Control.Etage.Internals

{-|
Sends an 'Impulse' from a 'Neuron'. 'Nerve' does not need to be conductive, 'Impulse' will be silently dropped in this case.
-}
sendFromNeuron :: Nerve from fromConductivity for forConductivity -> from -> IO ()
sendFromNeuron (Nerve (Axon chan) _) i = writeChan chan i
sendFromNeuron (Nerve NoAxon _) _ = return () -- we allow sending but ignore so that same Neuron defintion can be used on all kinds of Nerves

{-|
Gets an 'Impulse' from a 'Neuron'. It blocks until an 'Impulse' is available. 'Nerve' has to be conductive.
-}
getFromNeuron :: Nerve from AxonConductive for forConductivity -> IO from
getFromNeuron (Nerve (Axon chan) _) = readChan chan

{-|
Similar to 'getFromNeuron' just that it does not block if 'Impulse' is not available.
-}
maybeGetFromNeuron :: Nerve from AxonConductive for forConductivity -> IO (Maybe from)
maybeGetFromNeuron (Nerve (Axon chan) _) = tryReadChan chan

{-|
Gets all immediately available 'Impulse's from a 'Neuron'. There could be no 'Impulse's available and thus the result is an empty
list. Oldest 'Impulse' is the last in the list. 'Nerve' has to be conductive.
-}
slurpFromNeuron :: Nerve from AxonConductive for forConductivity -> IO [from]
slurpFromNeuron (Nerve (Axon chan) _) = slurpChan chan

{-|
Similar to 'slurpFromNeuron' but it waits for at least one 'Impulse'.
-}
waitAndSlurpFromNeuron :: Nerve from AxonConductive for forConductivity -> IO [from]
waitAndSlurpFromNeuron nerve = do
  oldest <- getFromNeuron nerve
  others <- slurpFromNeuron nerve
  return $ others ++ [oldest]

{-|
Returns a lazy list of 'Impulse's from a 'Neuron'. 'Nerve' has to be conductive.
-}
getContentsFromNeuron :: Nerve from AxonConductive for forConductivity -> IO [from]
getContentsFromNeuron (Nerve (Axon chan) _) = getChanContents chan

{-|
Sends all 'Impulses' from a given list to a 'Neuron'. 'Nerve' does not need to be conductive, 'Impulse's will be silently
dropped in this case.
-}
sendListFromNeuron :: Nerve from fromConductivity for forConductivity -> [from] -> IO ()
sendListFromNeuron nerve = mapM_ (sendFromNeuron nerve)

{-|
Sends an 'Impulse' to a 'Neuron'. 'Nerve' has to be conductive.
-}
sendForNeuron :: Nerve from fromConductivity for AxonConductive -> for -> IO ()
sendForNeuron (Nerve _ (Axon chan)) = writeChan chan

{-|
Gets an 'Impulse' for a 'Neuron'. It blocks until an 'Impulse' is available. 'Nerve' does not need to be conductive,
it will block indefinitely (until an exception) in this case.
-}
getForNeuron :: Nerve from fromConductivity for forConductivity -> IO for
getForNeuron (Nerve _ (Axon chan)) = readChan chan
getForNeuron (Nerve _ NoAxon) = waitForException

{-|
Similar to 'getForNeuron' just that it does not block if 'Impulse' is not available. 'Nerve' does not need to be conductive,
it will always return 'Nothing' in this case.
-}
maybeGetForNeuron :: Nerve from fromConductivity for forConductivity -> IO (Maybe for)
maybeGetForNeuron (Nerve _ (Axon chan)) = tryReadChan chan
maybeGetForNeuron (Nerve _ NoAxon) = return Nothing -- we allow getting but return Nothing so that same Neuron defintion can be used on all kinds of Nerves

{-|
Gets all immediately available 'Impulse's for a 'Neuron'. There could be no 'Impulse's available and thus the result is an empty
list. Oldest 'Impulse' is the last in the list. 'Nerve' does not need to be conductive, it will always return an empty list
in this case.
-}
slurpForNeuron :: Nerve from fromConductivity for forConductivity -> IO [for]
slurpForNeuron (Nerve _ (Axon chan)) = slurpChan chan
slurpForNeuron (Nerve _ NoAxon) = return [] -- we allow getting but return [] so that same Neuron defintion can be used on all kinds of Nerves

{-|
Similar to 'slurpForNeuron' but it waits for at least one 'Impulse'. 'Nerve' does not need to be conductive,
it will block indefinitely (until an exception) in this case.
-}
waitAndSlurpForNeuron :: Nerve from fromConductivity for forConductivity -> IO [for]
waitAndSlurpForNeuron nerve = do
  oldest <- getForNeuron nerve
  others <- slurpForNeuron nerve
  return $ others ++ [oldest]

{-|
Similar to 'waitAndSlurpForNeuron' but it will return only the newest 'Impulse' for every 'NeuronForImpulse' data type constructor.
This is the same as @head \<$\> waitAndSlurpForNeuron@ iff 'NeuronForImpulse' has only one constructor defined. Otherwise it can
return multiple 'Impulse's, for each constructor one.
-}
getNewestForNeuron :: Data for => Nerve from fromConductivity for forConductivity -> IO [for]
getNewestForNeuron nerve = do
  impulses <- waitAndSlurpForNeuron nerve
  return $ nubBy ((==) `on` toConstr) impulses

{-|
Returns a lazy list of 'Impulse's for a 'Neuron'. 'Nerve' does not need to be conductive,
it will block indefinitely (until an exception) in this case.
-}
getContentsForNeuron :: Nerve from fromConductivity for forConductivity -> IO [for]
getContentsForNeuron (Nerve _ (Axon chan)) = getChanContents chan
getContentsForNeuron (Nerve _ NoAxon) = waitForException

{-|
Sends all 'Impulses' from a given list to a 'Neuron'. 'Nerve' has to be conductive.
-}
sendListForNeuron :: Nerve from fromConductivity for AxonConductive -> [for] -> IO ()
sendListForNeuron nerve = mapM_ (sendForNeuron nerve)

-- First-in (oldest) element in the channel is last in the list
slurpChan :: Chan a -> IO [a]
slurpChan chan = slurpChan' []
  where slurpChan' cs = do
          mc <- tryReadChan chan
          case mc of
            Nothing -> return cs
            Just c  -> slurpChan' (c:cs)

{-|
'Neuron's can be mapped to capabilities (OS threads) in different ways. The best is to let Haskell decide the best capability
(and also move 'Neuron's among them as necessary) by using 'NeuronFreelyMapOnCapability' value, but sometimes because of an external
(FFI) library limitations you have to map 'Neuron' to a fixed capability, you can use 'NeuronMapOnCapability' for that.

Sometimes it is not important to which capability you map a 'Neuron', just that few 'Neuron's are mapped to the same. You can
use 'mkNeuronMapOnRandomCapability' to create such 'NeuronMapCapability' value.
-}
data NeuronMapCapability =
    NeuronMapOnCapability Int -- ^ Map a 'Neuron' to fixed capability.
  | NeuronFreelyMapOnCapability -- ^ Let Haskell decide on which capability is best to map a 'Neuron' at a given time.
  deriving (Eq, Ord, Read, Show)

{-|
Creates a 'NeuronMapOnCapability' value with a chosen capability picked by random. Useful when you have to map few 'Neuron's to the
same capability (because of an eternal (FFI) library limitations) but it does not matter to which one. So you create this value
and pass it as an option to all those 'Neuron's, making sure that they will return it with their 'getNeuronMapCapability' method.
For example, sometimes you have to assure that both your 'Neuron' and "Control.Etage.Worker" 'Neuron' are running on the same
capability so that you can correctly offload lengthly IO actions to it. This makes both 'Neuron's in fact still running in one
thread (which is often a limitation of external libraries), Haskell taking care of interleaving 'Neuron's IO actions.
-}
mkNeuronMapOnRandomCapability :: IO NeuronMapCapability
mkNeuronMapOnRandomCapability = do
  c <- randomRIO (1, numCapabilities)
  return $ NeuronMapOnCapability c

divideNeuron :: Neuron n => NeuronOptions n -> IO () -> IO NeuronId
divideNeuron options a = fork a
  where fork = case getNeuronMapCapability options of
                 NeuronFreelyMapOnCapability -> forkIO
                 NeuronMapOnCapability c     -> forkOnIO c

deriving instance Typeable1 NeuronFromImpulse
deriving instance Typeable1 NeuronForImpulse
deriving instance Typeable1 NeuronOptions

-- | A type class which defines common methods and data types of 'Neuron's.
class (Typeable n, Impulse (NeuronFromImpulse n), Impulse (NeuronForImpulse n)) => Neuron n where
  -- | A data type for 'Impulses' send from a 'Neuron'. 'Neuron' does not really need to use them.
  data NeuronFromImpulse n
  -- | A data type for 'Impulses' send for a 'Neuron'. 'Neuron' does not really need to use them.
  data NeuronForImpulse n
  -- | A data type for options. 'Neuron' does not really need to use them.
  data NeuronOptions n
  
  -- | Method which returns default values for options. By default returns 'undefined'.
  mkDefaultOptions :: IO (NeuronOptions n)
  
  -- | Method which returns how should 'Neuron' be mapped on capabilities (OS threads). By default returns
  -- 'NeuronFreelyMapOnCapability'.
  getNeuronMapCapability :: NeuronOptions n -> NeuronMapCapability
  
  -- | The first phase in a life-cycle of a 'Neuron' is to 'grow'. In this phase everything should be prepared and initialized.
  -- It returns a 'Neuron' value which is then passed to next phases. If you want to use 'NeuronOptions' also in those phases
  -- you should store them in the 'Neuron' value. By default returns 'undefined'.
  grow :: NeuronOptions n -> IO n
  -- | After 'grow'ing 'Neuron' 'live's. This is a phase in which it should read 'Impulse's from its 'Nerve' and send them back,
  -- as defined by its logic/purpose. Some 'Neuron's only read, some only send, some do both or none.
  --
  -- Most 'Neuron's do never finish this phase on its own (only by exception), but if your 'Neuron' does, consider using 'dissolving'
  -- at the end which initiates dissolving also elsewhere in the network (or in the parent 'Neuron', if it has one). Examples
  -- of such 'Neuron's are "Control.Etage.Timeout" and "Control.Etage.Sequence" (once a given sequence ends).
  --
  -- By default it blocks indefinitely (until an exception).
  live :: Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> n -> IO ()
  -- | In this phase everything should be cleaned up and deinitialized. If you have 'grow'n child 'Neuron's you should take care
  -- here to 'dissolve' them too. You can use 'detachAndWait' for that (or 'detachManyAndWait' if you have more of them).
  -- By default it does nothing.
  dissolve :: n -> IO ()

  -- | This method should take care of 'grow'ing a 'Neuron' with a given 'Nerve' 'attach'ed to it. It takes a function which
  -- changes default options and returns a 'LiveNeuron' value which can be used for 'detach'ing (and thus 'dissolve'-ing) the 'Neuron'.
  -- It should create a thread for a 'Neuron' to 'live' in and it should assure proper cleanup and 'dissolve'-ing.
  --
  -- By default it calls 'attach'' to do all that.
  attach :: (NeuronOptions n -> NeuronOptions n) -> Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> IO LiveNeuron

  mkDefaultOptions = return undefined

  getNeuronMapCapability _ = NeuronFreelyMapOnCapability

  grow _ = return undefined
  dissolve _ = return ()
  live _ _ = waitForException
  
  attach = attach'

{-|
Default implementation for 'attach' method. It takes a function which changes default options and returns a 'LiveNeuron' value
which can be used for 'detach'ing (and thus 'dissolve'-ing) the 'Neuron'.

It changes default options according to a given function, creates thread for a 'Neuron' to live in based on 'getNeuronMapCapability',
'grow's a 'Neuron', runs 'live' and prepares everything for cleanup with 'dissolve', whether because 'live' finished or because of an
exception. In the later case it rethrows an exception in the parent 'Neuron' (or in 'Incubation'). It also signals the 'Neuron'
has 'dissolve'd for 'detachAndWait' and 'detachManyAndWait'.
-}
attach' :: Neuron n => (NeuronOptions n -> NeuronOptions n) -> Nerve (NeuronFromImpulse n) fromConductivity (NeuronForImpulse n) forConductivity -> IO LiveNeuron
attach' optionsSetter nerve = mask_ $ do
  currentThread <- myThreadId
  dissolved <- newEmptySampleVar
  defOptions <- mkDefaultOptions
  let options = optionsSetter defOptions
  nid <- divideNeuron options $
           -- TODO: Remove unsafeUnmask in favor of forkIOWithUnmask when it will be available
           bracket (grow options) dissolve (unsafeUnmask . live nerve) `catches` [
               Handler (\(_ :: DissolveException) -> return ()), -- we ignore DissolveException
               Handler (\(e :: SomeException) -> uninterruptible $ throwTo currentThread e)
             ] `finally` uninterruptible (writeSampleVar dissolved ())
  return $ LiveNeuron dissolved nid

{-|
An exception which initiates 'dissolve'-ing of a 'Neuron'. Should be thrown inside the 'Neuron' with passing its 'Neuron' value as
argument (as passed to 'live' method). For throwing outside the 'Neuron' use 'DissolveException' (or simply 'detach' and others).
-}
data DissolvingException = DissolvingException String deriving (Show, Typeable)

instance Exception DissolvingException

{-|
Initiates 'dissolve'-ing of a 'Neuron' by throwing a 'DissolvingException'. To be used inside a 'Neuron' to maybe prematurely
finish its life but more importantly to initiate 'dissolve'-ing in the parent 'Neuron' (or in 'Incubation'). As an argument
it is accustomed to pass a 'Neuron' value as passed to 'live' method.
-}
dissolving :: Show n => n -> IO a
dissolving n = throwIO $ DissolvingException (show n)

{-|
An exception which initiates 'dissolve'-ing of a 'Neuron'. Should be thrown outside the 'Neuron' to the 'Neuron'. For
throwing inside the 'Neuron' use 'DissolvingException' (or simply 'dissolving').
-}
data DissolveException = DissolveException deriving (Show, Typeable)

instance Exception DissolveException

{-|
Initiates 'dissolve'-ing of a 'Neuron' by throwing a 'DissolveException'. To be used outside of a 'Neuron'.
-}
detach :: LiveNeuron -> IO ()
detach (LiveNeuron _ neuronId) = mask_ . uninterruptible $ throwTo neuronId DissolveException

{-|
Similar to 'detachAndWait' but it also waits 'Neuron' to finish 'dissolve'-ing.
-}
detachAndWait :: LiveNeuron -> IO ()
detachAndWait n = detachManyAndWait [n]

{-|
Similar to 'detach' but for many 'Neuron's at the same time. It initiates 'dissolve'-ing in the list order.
-}
detachMany :: [LiveNeuron] -> IO ()
detachMany = mask_ . mapM_ detach

{-|
Similar to 'detachAndWait' but for many 'Neuron's at the same time. It first initiates 'dissolve'-ing in the list order and then
wait for all 'Neuron's to finish 'dissolve'-ing.
-}
detachManyAndWait :: [LiveNeuron] -> IO ()
detachManyAndWait neurons = mask_ $ do
  detachMany neurons
  mapM_ (\(LiveNeuron d _) -> uninterruptible $ readSampleVar d) neurons

-- Some IO operations are interruptible, better than to make them uninterruptible (which can cause deadlocks) we simply retry interrupted operation
-- For this to really work all interruptible operations should be wrapped like this (so it is not good idea to use IO operations in such code sections)
uninterruptible :: IO a -> IO a
uninterruptible a = mask_ $ a `catch` (\(_ :: SomeException) -> uninterruptible a)

-- | This type class defines a method for translating between 'Impulse' types.
class (Impulse i, Impulse j) => ImpulseTranslator i j where
  -- | 'translate' gets an 'Impulse' of one type and returns a list of 'Impulses' of another type.
  --
  -- 'Impulse's should be translated meaningfully, translating values as possible and filling others with reasonable defaults.
  -- Timestamp should be just copied (translation should be seen as an instantaneous operation as it is a byproduct of type
  -- constraints and chosen description format of 'Impulse's and not something found otherwise in a network.
  -- Time spend in translation should be seen as a part of time spend in sending of an 'Impulse' along a 'Nerve'.
  --
  -- One 'Impulse' can be translated into multiple other 'Impulse's as sometimes some 'Impulse's are higher level than other.
  -- (Translating multiple 'Impulse's into one 'Impulse' requires keeping a state and should be done in a 'Neuron'.) The order is
  -- important as first 'Impulse's in the list are send first along a 'Nerve'.
  translate :: i -> [j]

{-|
Function which can be used as an argument to 'growNeuron' or 'attach' which leaves default options as they are.

In fact it is just an 'id'entity function.
-}
defaultOptions :: Neuron n => NeuronOptions n -> NeuronOptions n
defaultOptions = id

{-|
Helper function which does some common initialization. Currently it sets 'stderr' buffering to 'LineBuffering' so that when
multiple 'Neuron's print to 'stderr' output is not mixed. It also installs handlers for 'keyboardSignal' and 'softwareTermination'
signals so that cleanup in 'Incubation' works as expected.
-}
prepareEnvironment :: IO ()
prepareEnvironment = do
  hSetBuffering stderr LineBuffering
  
  mainThreadId <- myThreadId
  
  -- TODO: User interrupt sometimes hangs dissolving (does it still in GHC 7.0?)
  _ <- installHandler keyboardSignal (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigINT
  _ <- installHandler softwareTermination (Catch (throwTo mainThreadId UserInterrupt)) Nothing -- sigTERM
  
  return ()

{-|
Translates (if necessary 'ImpulseTranslator' exists) an 'Impulse' and sends translation to 'Neuron'.
-}
translateAndSend :: ImpulseTranslator i for => Nerve from fromConductivity for AxonConductive -> i -> IO ()
translateAndSend nerve i = mapM_ (sendForNeuron nerve) $ translate i

{-|
Returns current time. Useful when creating new 'Impulse's.
-}
getCurrentImpulseTime :: IO ImpulseTime
getCurrentImpulseTime = getPOSIXTime

{-|
This function defines equality between 'Impulse's as equality of 'impulseTime' and 'impulseValue' values. Useful for 'Neuron's which
operate on all types of 'Impulse's and want 'Eq' defined on their 'Impulse's. Examples of such 'Neuron's are "Control.Etage.Dump"
and "Control.Etage.Function".
-}
impulseEq :: (Impulse i, Impulse j) => i -> j -> Bool
impulseEq a b = impulseTime a == impulseTime b && impulseValue a == impulseValue b

{-|
This function defines ordering between 'Impulse's as ordering first by 'impulseTime' values and then by 'impulseValue' values.
Useful for 'Neuron's which operate on all types of 'Impulse's and want 'Ord' defined on their 'Impulse's. Examples of such
'Neuron's are "Control.Etage.Dump" and "Control.Etage.Function".
-}
impulseCompare :: (Impulse i, Impulse j) => i -> j -> Ordering
impulseCompare a b = (impulseTime a, impulseValue a) `compare` (impulseTime b, impulseValue b)
