{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Types where

import Data.Csv
import GHC.Generics (Generic)

data LifeEvent
    = Germ
    | Flower
    | SeedSet
    deriving (Generic, Show)

instance FromField LifeEvent

instance ToField LifeEvent where
  toField Germ = "Germ"
  toField Flower = "Flower"
  toField SeedSet = "SeedSet"

data Event = Event
    { timeE :: !Double
    , pid :: !Int
    , typeE :: !LifeEvent
    } deriving (Generic, Show)

instance FromNamedRecord Event
instance ToNamedRecord Event
instance DefaultOrdered Event


