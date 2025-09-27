{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import System.Signal


wsApp :: ServerApp
wsApp pending = do
    conn <- acceptRequest pending
    putStrLn "WebSocket connection established"

    flip finally disconnect $ forever $ do
        msg <- receiveData conn
        T.putStrLn $ "Received: " <> T.pack (show (msg :: Text))
        sendTextData conn msg
  where
    disconnect = putStrLn "WebSocket connection closed"

application :: Application
application req respond
    | pathInfo req == ["socket"] = websocketsOr defaultConnectionOptions wsApp backupApp req respond
    | otherwise = backupApp req respond
  where
    backupApp :: Application
    backupApp _ responseFunc = responseFunc $ responseLBS status404 [] "Not found"

main :: IO ()
main = do
    putStrLn "Starting WebSocket echo server on port 8080"
    putStrLn "WebSocket endpoint available at: ws://localhost:8080/socket"

    let settings = defaultSettings
                 & W.setInstallShutdownHandler (\closeSocket -> void $ do
                                                   void $ installHandler sigTERM (\_ -> putStrLn "Shutting down due to SIGTERM..." >> closeSocket)
                                                   void $ installHandler sigINT (\_ -> putStrLn "Shutting down due to SIGINT..." >> closeSocket)
                                               )
                 & setGracefulShutdownTimeout (Just 1)
                 & setPort 8080

    runSettings settings application
