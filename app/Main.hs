{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Format (defaultTimeLocale, formatTime)
import MyDatabase
  ( connect,
    createTable,
    exampleEvent,
    insertEvent,
  )
import MyEvent (Event (Event))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant
  ( Application,
    Get,
    JSON,
    Proxy (..),
    Server,
    serve,
    (:<|>) ((:<|>)),
    type (:>),
  )

-- Make the Servant API
type API = Get '[JSON] String :<|> "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy

server :: Server API
server =
  do
    liftIO $ putStrLn "Received a request"
    return "Hello, Haskell!"
    :<|> return "Hello, Servant!"

app :: Application
app = serve api server

port :: Int
port = 8080

printer :: Event -> String
printer (Event name timestamp data') = name ++ " " ++ formatTime defaultTimeLocale "%Y-%m-%d" timestamp ++ " " ++ data'

exampleEvent :: Event
exampleEvent = Event "example" (read "2020-01-01 00:00:00") "example data"

-- | Start database connection and run the application
startApp :: IO ()
startApp = do
  putStrLn "Running on http://localhost:8080"
  conn <- connect
  putStrLn "Connected to database"
  createTable conn
  insertEvent conn MyDatabase.exampleEvent
  withStdoutLogger $ \aplogger ->
    let settings = setPort port $ setLogger aplogger defaultSettings
     in runSettings settings app

-- | Main entry point
main :: IO ()
main = startApp

-- main :: IO ()
-- main = do
--   putStrLn "Running on http://localhost:8080"
--   withStdoutLogger $ \aplogger ->
--     let settings = setPort port $ setLogger aplogger defaultSettings
--      in runSettings settings app
