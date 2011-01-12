{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Chan
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Unbounded channels.
--
-----------------------------------------------------------------------------

-- Changes: Eq derived on Chan.

module Control.Etage.Chan
  ( 
          -- * The 'Chan' type
        Chan,                   -- abstract

          -- * Operations
        newChan,                -- :: IO (Chan a)
        writeChan,              -- :: Chan a -> a -> IO ()
        readChan,               -- :: Chan a -> IO a
        tryReadChan,            -- :: Chan a -> IO (Maybe a)
        dupChan,                -- :: Chan a -> IO (Chan a)
        unGetChan,              -- :: Chan a -> a -> IO ()
        isEmptyChan,            -- :: Chan a -> IO Bool

          -- * Stream interface
        getChanContents,        -- :: Chan a -> IO [a]
        writeList2Chan,         -- :: Chan a -> [a] -> IO ()
   ) where

import Prelude

import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.IORef
import Data.Typeable

import Control.Exception.Base

-- A channel is represented by two @MVar@s keeping track of the two ends
-- of the channel contents,i.e.,  the read- and write ends. Empty @MVar@s
-- are used to handle consumers trying to read from an empty channel.

-- |'Chan' is an abstract type representing an unbounded FIFO channel.
data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))
        Int
 deriving (Typeable)

instance Eq (Chan a) where
  (Chan _ _ i') == (Chan _ _ i'') = i' == i''

instance Ord (Chan a) where
  (Chan _ _ i') `compare` (Chan _ _ i'') = i' `compare` i''

globalChanIndex :: IORef Int
{-# NOINLINE globalChanIndex #-}
globalChanIndex = unsafePerformIO (newIORef 0)

type Stream a = MVar (ChItem a)

data ChItem a = ChItem a (Stream a)

-- See the Concurrent Haskell paper for a diagram explaining the
-- how the different channel operations proceed.

-- @newChan@ sets up the read and write end of a channel by initialising
-- these two @MVar@s with an empty @MVar@.

-- |Build and returns a new instance of 'Chan'.
newChan :: IO (Chan a)
newChan = do
   hole  <- newEmptyMVar
   readVar  <- newMVar hole
   writeVar <- newMVar hole
   index <- atomicModifyIORef globalChanIndex (\i -> (i + 1, i))
   return (Chan readVar writeVar index)

-- To put an element on a channel, a new hole at the write end is created.
-- What was previously the empty @MVar@ at the back of the channel is then
-- filled in with a new stream element holding the entered value and the
-- new hole.

-- |Write a value to a 'Chan'.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar _) val = do
  new_hole <- newEmptyMVar
  modifyMVar_ writeVar $ \old_hole -> do
    putMVar old_hole (ChItem val new_hole)
    return new_hole

-- |Read the next value from the 'Chan'.
readChan :: Chan a -> IO a
readChan (Chan readVar _ _) = do
  modifyMVar readVar $ \read_end -> do
    (ChItem val new_read_end) <- readMVar read_end
        -- Use readMVar here, not takeMVar,
        -- else dupChan doesn't work
    return (new_read_end, val)

{-|
  A semi-non-blocking version of 'readMVar'. The 'tryReadMVar' function returns immediately, with 'Nothing' if the 'MVar' was empty, or
  @'Just' a@ if the 'MVar' was full with contents @a@, after it put the value back (it can block at this stage).
-}
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar m =
  mask_ $ do
    a <- tryTakeMVar m
    case a of
      Nothing -> return Nothing
      Just a' -> do
        putMVar m a'
        return $ Just a'

{-|
  A semi-non-blocking version of 'modifyMVar'. The 'tryModifyMVar' function returns immediately, with 'Nothing' if the 'MVar' was empty, or
  behave as 'modifyMVar' otherwise. This means that it can still block while putting the original (on exception) or new value (otherwise) back.
-}
{-# INLINE tryModifyMVar #-}
tryModifyMVar :: MVar a -> (a -> IO (a, Maybe b)) -> IO (Maybe b)
tryModifyMVar m io =
  mask $ \restore -> do
    a <- tryTakeMVar m
    case a of
      Nothing -> return Nothing
      Just a' -> do
        (a'', b) <- restore (io a') `onException` putMVar m a'
        putMVar m a''
        return b

-- |A non-blocking version of 'readChan'. The 'tryReadChan' function returns immediately, with 'Nothing' if the 'Chan' was empty or would
-- block, or @'Just' a@ with the next value from the 'Chan', otherwise.
tryReadChan :: Chan a -> IO (Maybe a)
tryReadChan (Chan readVar _ _) = do
  tryModifyMVar readVar $ \read_end -> do
    item <- tryReadMVar read_end
        -- Use tryReadMVar here, not tryTakeMVar,
        -- else dupChan doesn't work
    case item of
      Nothing -> return (read_end, Nothing)
      Just (ChItem val new_read_end) -> return (new_read_end, Just val)

-- |Duplicate a 'Chan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar index) = do
   hole       <- readMVar writeVar
   newReadVar <- newMVar hole
   return (Chan newReadVar writeVar index)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readVar _ _) val = do
   new_read_end <- newEmptyMVar
   modifyMVar_ readVar $ \read_end -> do
     putMVar new_read_end (ChItem val read_end)
     return new_read_end
{-# DEPRECATED unGetChan "if you need this operation, use Control.Concurrent.STM.TChan instead.  See http://hackage.haskell.org/trac/ghc/ticket/4154 for details" #-}

-- |Returns 'True' if the supplied 'Chan' is empty.
isEmptyChan :: Chan a -> IO Bool
isEmptyChan (Chan readVar writeVar _) = do
   withMVar readVar $ \r -> do
     w <- readMVar writeVar
     let eq = r == w
     eq `seq` return eq
{-# DEPRECATED isEmptyChan "if you need this operation, use Control.Concurrent.STM.TChan instead.  See http://hackage.haskell.org/trac/ghc/ticket/4154 for details" #-}

-- Operators for interfacing with functional streams.

-- |Return a lazy list representing the contents of the supplied
-- 'Chan', much like 'System.IO.hGetContents'.
getChanContents :: Chan a -> IO [a]
getChanContents ch
  = unsafeInterleaveIO (do
        x  <- readChan ch
        xs <- getChanContents ch
        return (x:xs)
    )

-- |Write an entire list of items to a 'Chan'.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)
