{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MyEvent where

import Data.Aeson
import Data.Time (UTCTime)
import GHC.Generics

data Event = Event
  { eventName :: String,
    eventTimestamp :: UTCTime,
    eventData :: String
  }
  deriving (Show, Generic)

instance ToJSON Event

instance FromJSON Event
