{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Types where

import Data.Csv
import GHC.Generics (Generic)

data LifeEvent
    = Germ
    | Flower
    | SeedSet
    deriving (Generic, Show)

instance FromField LifeEvent where
  parseField e
       | e == "Germ" = pure Germ
       | e == "Flower" = pure Flower
       | e == "SeedSet" = pure SeedSet

instance ToField LifeEvent where
  toField Germ = "Germ"
  toField Flower = "Flower"
  toField SeedSet = "SeedSet"

data Event = Event
    { timeE :: !Double
    , pid :: !Int
    , typeE :: !LifeEvent
    , nSeeds :: !Int
    , nPlants :: !Int
    , nFPlants :: !Int
    } deriving (Generic, Show)

instance FromNamedRecord Event
instance ToNamedRecord Event
instance DefaultOrdered Event


