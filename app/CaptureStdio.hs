{-# LANGUAGE CPP #-}
module CaptureStdio (captureStdErr, captureStdout) where

import qualified Control.Exception as E
import GHC.IO.Handle
import System.IO (stdin, stderr, stdout)
import qualified System.Process as Process
import qualified System.IO.Silently as Silently


captureStdErr :: IO a -> IO (String, a)
captureStdout :: IO a -> IO (String, a)
#ifdef WINDOWS
captureStdErr = Silently.hCapture [stderr]
captureStdout = Silently.hCapture [stdout]
#else
captureStdErr = captureHandleString stderr
captureStdout = captureHandleString stdout
#endif

captureHandleString :: Handle -> IO a -> IO (String, a)
captureHandleString origHandle act = do
  (readEnd, writeEnd) <- Process.createPipe
  -- TODO: Read the actual content
  res <- goBracket act writeEnd origHandle
  output <- hGetContents' readEnd
  pure (output, res)


-- stdoutToStdErr :: IO a -> IO a
-- stdoutToStdErr act = goBracket act stderr stdout

-- | Copy a handle to another within a bracket
goBracket :: IO a -> Handle -> Handle -> IO a
goBracket go tmpHandle h = do
  buffering <- hGetBuffering h
  let redirect = do
        old <- hDuplicate h
        hDuplicateTo tmpHandle h
        return old
      restore old = do
        hDuplicateTo old h
        hSetBuffering h buffering
        hClose old
  E.bracket redirect restore (const go)
