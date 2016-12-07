{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import Network.Simple.TCP

main :: IO ()
main = do
  serve "127.0.0.1" "11235" $ \(socket, socketAddr) -> do
    putStrLn $ "Connection established to client: " ++ show socketAddr
    spammerServer socket socketAddr
    ioError $ userError "ERRAWR!"
  putStrLn "Server stopped listening."

receiverServer :: Socket -> SockAddr -> IO ()
receiverServer socket socketAddr =
  let
    recvRest = do
      maybeChunk <- recv socket 2
      case maybeChunk of
        Nothing -> do
          putStrLn $ "Lost connection to client: " ++ show socketAddr
        Just chunk -> do
          putStrLn $ "Received chunk: " ++ show chunk
          recvRest
  in recvRest

spammerServer :: Socket -> SockAddr -> IO ()
spammerServer socket socketAddr =
  let
  sendRest x = do
    if x > 0
      then do
        putStrLn "Sending message.."
        send socket "Buy our product!\n"
        threadDelay 5000000
        sendRest (x - 1)
      else
        return ()
  in sendRest 3
