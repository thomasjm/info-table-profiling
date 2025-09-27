module Main (main) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Network.HTTP.Types.Status
import Control.Exception (finally)
import Control.Monad (forever)

wsApp :: ServerApp
wsApp pending = do
    conn <- acceptRequest pending
    putStrLn "WebSocket connection established"
    
    flip finally disconnect $ forever $ do
        msg <- receiveData conn
        putStrLn $ "Received: " ++ show (msg :: String)
        sendTextData conn msg
  where
    disconnect = putStrLn "WebSocket connection closed"

application :: Application
application req respond
    | pathInfo req == ["socket"] = websocketsOr defaultConnectionOptions wsApp backupApp req respond
    | otherwise = backupApp req respond
  where
    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status404 [] "Not found"

main :: IO ()
main = do
    putStrLn "Starting WebSocket echo server on port 8080"
    putStrLn "WebSocket endpoint available at: ws://localhost:8080/socket"
    run 8080 application
