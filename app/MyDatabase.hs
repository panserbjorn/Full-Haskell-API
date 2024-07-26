{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module MyDatabase where

import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), ToRow, connectPostgreSQL, execute, execute_, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import qualified Database.PostgreSQL.Simple.FromRow as Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import MyEvent (Event (..))

-- | Convert an event to a row for the database
instance ToRow Event where
  toRow :: Event -> [Database.PostgreSQL.Simple.ToField.Action]
  toRow event = [toField (eventName event), toField (eventTimestamp event), toField (eventData event)]

-- | Convert a row from the database to an event
instance FromRow Event where
  fromRow :: Database.PostgreSQL.Simple.Internal.RowParser Event
  fromRow = Event <$> field <*> field <*> field

-- | Insert an event into the database
insertEvent :: Connection -> Event -> IO ()
insertEvent conn event = do
  execute conn "INSERT INTO events (name, timestamp, data) VALUES (?, ?, ?)" event
  return ()

-- | Get all events from the database
getEvents :: Connection -> IO [Event]
getEvents conn = do
  query_ conn "SELECT name, timestamp, data FROM events" :: IO [Event]

-- | Get all events from the database that match a given name
getEventsByName :: Connection -> String -> IO [Event]
getEventsByName conn name = do
  query conn "SELECT name, timestamp, data FROM events WHERE name = ?" (Only name) :: IO [Event]

-- | Get all events from the database that occurred after a given time
getEventsAfter :: Connection -> UTCTime -> IO [Event]
getEventsAfter conn time = do
  query conn "SELECT name, timestamp, data FROM events WHERE timestamp > ?" (Only time) :: IO [Event]

-- | Get all events from the database that occurred before a given time
getEventsBefore :: Connection -> UTCTime -> IO [Event]
getEventsBefore conn time = do
  query conn "SELECT name, timestamp, data FROM events WHERE timestamp < ?" (Only time) :: IO [Event]

-- | Get all events from the database that occurred between two times
getEventsBetween :: Connection -> UTCTime -> UTCTime -> IO [Event]
getEventsBetween conn start end = do
  query conn "SELECT name, timestamp, data FROM events WHERE timestamp > ? AND timestamp < ?" (start, end) :: IO [Event]

-- | Delete all events from the database
deleteEvents :: Connection -> IO ()
deleteEvents conn = do
  execute_ conn "DELETE FROM events"
  return ()

-- | Delete all events from the database that match a given name
deleteEventsByName :: Connection -> String -> IO ()
deleteEventsByName conn name = do
  execute conn "DELETE FROM events WHERE name = ?" (Only name)
  return ()

-- | Establish a connection to the database
connect :: IO Connection
connect = connectPostgreSQL "dbname=postgres"

-- | Create the events table in the database
createTable :: Connection -> IO ()
createTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS events (name TEXT, timestamp TIMESTAMP, data TEXT)"
  return ()

-- | Drop the events table from the database
dropTable :: Connection -> IO ()
dropTable conn = do
  execute_ conn "DROP TABLE IF EXISTS events"
  return ()

-- | Clear the events table in the database
clearTable :: Connection -> IO ()
clearTable conn = do
  execute_ conn "TRUNCATE TABLE events"
  return ()

-- | Initialize the database
initialize :: IO ()
initialize = do
  conn <- connect
  createTable conn
  insertEvent conn exampleEvent
  return ()

-- | An example event to insert into the database
exampleEvent :: Event
exampleEvent = Event "example" (read "2020-01-01 00:00:00") "example data"

-- | A list of example events to insert into the database
exampleEvents :: [Event]
exampleEvents =
  [ Event "event1" (read "2020-01-01 00:00:00") "event1 data",
    Event "event2" (read "2020-01-02 00:00:00") "event2 data",
    Event "event3" (read "2020-01-03 00:00:00") "event3 data"
  ]

-- | Insert the example events into the database
insertExampleEvents :: Connection -> IO ()
insertExampleEvents conn = mapM_ (insertEvent conn) exampleEvents
