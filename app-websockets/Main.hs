{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.WebSockets


wsApp :: ServerApp
wsApp pending = do
  conn <- acceptRequest pending
  putStrLn "WebSocket connection established"

  flip finally (putStrLn "WebSocket connection closed") $ forever $ do
    msg <- receiveData conn
    T.putStrLn $ "Received: " <> T.pack (show (msg :: Text))
    sendTextData conn msg

main :: IO ()
main = do
  putStrLn "Starting WebSocket only echo server on port 8080"
  putStrLn "WebSocket endpoint available at: ws://localhost:8080/socket"

  runServer "127.0.0.1" 8080 wsApp
