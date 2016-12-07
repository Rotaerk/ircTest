{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Applicative
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Network.IRC as IRC
import Network.Simple.TCP
import Pipes
import Pipes.Core
import Pipes.Safe
import qualified Pipes.Attoparsec as PAtto
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as P
import qualified Pipes.Network.TCP.Safe as PN

userName :: B.ByteString
userName = "Rotaerk"

main :: IO ()
main = runSafeT $ runEffect $ ircEventProducer "127.0.0.1" "6667" 4096 >-> ircEventConsumer

data IrcCommand = Send B.ByteString deriving (Show)
data IrcEvent =
  Connected (Socket, SockAddr) |
  Disconnected |
  ReceivedMessage IRC.Message |
  ParseFailure PAtto.ParsingError
  deriving (Show)

ircEventProducer :: HostName -> ServiceName -> Int -> Producer IrcEvent (SafeT IO) ()
ircEventProducer hostName port bytesPerChunk = do
  PN.connect hostName port $ \(socket, socketAddr) -> do
    yield $ Connected (socket, socketAddr)
    parserResult <- PAtto.parsed ircMessage (PN.fromSocket socket bytesPerChunk) >-> P.map ReceivedMessage
    case parserResult of
      Left (e, _) -> yield (ParseFailure e)
      Right _ -> yield Disconnected

ircMessage :: Parser IRC.Message
ircMessage =
  Message <$>
    option Nothing (Just <$> (IRC.prefix <* IRC.spaces))
    <*> IRC.command
    <*> many (IRC.spaces *> IRC.parameter)
    <* optional IRC.crlf
    <?> "ircMessage"

ircEventConsumer :: Consumer IrcEvent (SafeT IO) ()
ircEventConsumer = disconnectedState
  where
    disconnectedState = do
      event <- await
      case event of
        Connected (socket, socketAddr) -> do
          liftIO $ putStrLn $ "Connected to: " ++ show socketAddr
          commandThreadId <- liftIO $ forkIO $ runSafeT $ runEffect $ ircCommandPipeline socket
          liftIO $ do
            sendMessageTo socket $ nick userName
            sendMessageTo socket $ user userName "0" "*" ":Matt"
          connectedState socket socketAddr commandThreadId
        _ -> do
          liftIO $ putStrLn $ "Unexpected event while disconnected: " ++ show event
          disconnectedState
    connectedState socket socketAddr commandThreadId =
      let
        disconnectedState' = do
          liftIO $ killThread commandThreadId
          disconnectedState
        connectedState' = do
          event <- await
          case event of
            ReceivedMessage message -> do
              liftIO $ do
                putStrLn $ "Received message: " ++ show message
                case message of
                  Message _ "PING" [key] -> sendMessageTo socket $ pong key
                  _ -> return ()
              connectedState'
            ParseFailure e -> do
              liftIO $ putStrLn $ "Parse error: " ++ show e
              disconnectedState'
            Disconnected -> do
              liftIO $ putStrLn "Disconnected"
              disconnectedState'
            _ -> do
              liftIO $ putStrLn $ "Unexpected event while connected: " ++ show event
              connectedState'
      in connectedState'

sendMessageTo :: Socket -> Message -> IO ()
sendMessageTo socket message = do
  putStrLn $ "Sending message: " ++ show message
  send socket $ B.append (encode message) "\r\n"
  putStrLn "Sent."

ircCommandConsumer :: Socket -> Consumer IrcCommand (SafeT IO) ()
ircCommandConsumer socket =
  let
    loop = do
      command <- await
      case command of
        Send bs ->
          liftIO $ do
            putStrLn $ "Sending: " ++ BC8.unpack bs
            send socket bs
            putStrLn $ "Done Sending."
      loop
  in loop

ircCommandPipe :: Pipe String IrcCommand (SafeT IO) ()
ircCommandPipe = P.map (Send . BC8.pack . (++ "\r\n"))

ircCommandPipeline :: Socket -> Effect (SafeT IO) ()
ircCommandPipeline socket = P.stdinLn >-> ircCommandPipe >-> ircCommandConsumer socket

{-
greeterClient :: Socket -> SockAddr -> IO ()
greeterClient socket socketAddr = do
  send socket "Howdy!"
  threadDelay 5000000

receiverClient :: Socket -> SockAddr -> IO ()
receiverClient socket socketAddr =
  let
    recvRest = do
      putStrLn "Requesting data"
      maybeChunk <- recv socket 4096
      case maybeChunk of
        Nothing -> do
          putStrLn $ "Lost connection to server: " ++ show socketAddr
        Just chunk -> do
          putStrLn $ "Chunk received: " ++ BC8.unpack chunk
          putStrLn "Waiting 5 seconds..."
          threadDelay 5000000
          recvRest
  in recvRest

spammerClient :: Socket -> SockAddr -> IO ()
spammerClient socket socketAddr =
  let
    spam = do
      putStrLn "Sending message"
      send socket "YOU THERE?!"
      putStrLn "Waiting 5 seconds..."
      threadDelay 5000000
      spam
  in spam
-}
