{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module PlantL where

import           Agent
import           Chromar
import           Data.Fixed
import           Data.List
import           Env
import           Params

log' t = 1.0 / (1.0 + exp (-100.0 * (t - 1000.0)))

logf' :: Double -> Double
logf' t = 1.0 / (1.0 + exp (-100.0 * (t - 2604.0)))

logs' :: Double -> Double
logs' t = 1.0 / (1.0 + exp (-100.0 * (t - 8448.0)))

thrmFinal = 2604

plantD = Observable { name = "plantD",
                      gen = sumM dg . select isPlant }

eplantD = Observable { name = "plantD",
                      gen = sumM dg . select isEPlant }

plantTa = Observable { name = "plantTa",
                       gen = sumM ta . select isEPlant }

sdg =
    Observable
    { name = "sdeg"
    , gen = sumM sdeg . select isEPlant
    }

$(return [])

----- rules -------

dev =
    [rule| Seed{attr=atr, dg=d, art=a} -->
           Seed{attr=atr, dg = d + (htu time a (psi atr)), art=a + (arUpd moist temp)}
           @1.0
   |]

germ =
  [rule| Seed{mass=m, attr=atr, dg=d, art=a} -->
         EPlant{sdeg=0.0, thrt=0.0, attr=atr, dg=0.0, wct=0.0} @log' d
  |]

devep =
    [rule| EPlant{attr=atr, thrt=tt, dg=d, wct=w} -->
           EPlant{attr=atr, thrt=tt+(temp / 24.0),
                  dg=d+ptu* fp (wcUpd time w) (fi atr), wct=wcUpd time w}
           @1.0 |]

transp =
    [rule|
        EPlant{attr=atr, dg=d, wct=w} -->
        FPlant{attr=atr, dg=0.0}
        @logf' d
    |]

devfp =
    [rule| FPlant{dg=d} --> FPlant{dg=d+disp} @1.0 |]

transfp =
    [rule|
         FPlant{attr=atr, dg=d} -->
         Seed{mass=1.6e-5, attr=atr, dg=0.0, art=0.0}
         @logs' d
   |]

----------

hasFlowered :: Multiset Agent -> Bool
hasFlowered mix = (sumM dg . select isEPlant) mix < 2604

hasGerminated :: Multiset Agent -> Bool
hasGerminated mix=  (sumM dg . select isSeed) mix < 1000
