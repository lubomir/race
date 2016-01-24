{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async (Concurrently (..))
import qualified Data.ByteString          as B
import           Data.Conduit             (($$), (=$))
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..),
                                           CreateProcess (..), StdStream (..),
                                           UseProvidedHandle (..), shell,
                                           streamingProcess,
                                           waitForStreamingProcess)
import           Data.Monoid              ((<>))
import           System.Environment       (getArgs)
import           System.Posix.IO          (fdToHandle)
import           System.Posix.Terminal    (openPseudoTerminal)

data Msg = Quit | Msg Int B.ByteString

{-| Run @cmd@ and send both its output and stdout into the @chan'@.
 -}
runProcess :: Chan Msg -> Int -> String -> IO ()
runProcess chan i cmd = do
    (master', slave') <- openPseudoTerminal
    master <- fdToHandle master'
    slave <- fdToHandle slave'

    let cp = (shell cmd) {std_out = UseHandle master}

    (ClosedStream, UseProvidedHandle, fromProcessErr, cph) <-
        streamingProcess cp

    let output h = CB.sourceHandle h $$ CB.lines =$ CL.mapM_
            (writeChan chan . Msg i)

    _ <- runConcurrently $
        Concurrently (output slave) *>
        Concurrently (output fromProcessErr) *>
        Concurrently (waitForStreamingProcess cph)

    writeChan chan Quit


{-| Wrap a bytestring in ANSI color sequences.
 -}
colored :: Int -> B.ByteString -> B.ByteString
colored i d = let col = colors !! i
              in "\ESC[" <> col <> "m" <> d <> "\ESC[0m\n"
  where
    colors = cycle ["34", "36", "35", "32", "33", "31"]

{-| Read everything from the channel and print it. This function returns only
 - when all processes send a Quit message.
 -}
reader :: Chan Msg -> Int -> IO ()
reader _ 0 = return ()
reader chan num = do
    msg <- readChan chan
    case msg of
        Quit -> reader chan (num - 1)
        Msg i d -> do
            B.putStr $ colored i d
            reader chan num

main :: IO ()
main = do
    args <- getArgs
    readEnd <- newChan
    writeEnd <- dupChan readEnd

    mapM_ (forkIO . uncurry (runProcess writeEnd)) (zip [0..] args)

    reader readEnd (length args)
