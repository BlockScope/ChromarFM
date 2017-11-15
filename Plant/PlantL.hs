{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module PlantL where

import           Agent
import           Chromar
import           Data.Fixed
import           Data.List
import           Env
import           Params

-- log' t = 1.0 / (1.0 + exp (-100.0 * (t - 1000.0)))

-- logf' :: Double -> Double
-- logf' t = 1.0 / (1.0 + exp (-100.0 * (t - 2604.0)))

-- logs' :: Double -> Double
-- logs' t = 1.0 / (1.0 + exp (-100.0 * (t - 8448.0)))

log' t =
    if t > 1000.0
        then 1.0
        else 0.0

logf' t =
    if t > 2604.0
        then 1.0
        else 0.0

logs' t =
    if t > 8448.0
        then 1.0
        else 0.0

thrmFinal = 2604

$(return [])

----- rules -------

dev =
    [rule| Seed{attr=atr, dg=d, art=a} -->
           Seed{attr=atr, dg = d + 1*(htu time a (psi atr)), art=a + 1*(arUpd moist temp)}
           @1.0/1.0
   |]

germ =
  [rule| Seed{mass=m, attr=atr, dg=d, art=a} -->
         EPlant{sdeg=0.0, thrt=0.0, attr=atr, dg=0.0, wct=0.0} @log' d / 1.0
  |]

devep =
    [rule| EPlant{attr=atr, thrt=tt, dg=d, wct=w} -->
           EPlant{attr=atr, thrt=tt+(temp / 1.0),
                  dg=d+1*ptu* fp (wcUpd time w) (fi atr), wct=1*wcUpd time w}
           @1.0/1.0 |]

transp =
    [rule|
        EPlant{attr=atr, dg=d, wct=w} -->
        FPlant{attr=atr, dg=0.0}
        @logf' d/1.0
    |]
    
devfp =
    [rule| FPlant{dg=d} --> FPlant{dg=d+1.0*disp} @1.0/1.0 |]

transfp =
    [rule|
         FPlant{attr=atr, dg=d} -->
         Seed{mass=1.6e-5, attr=atr, dg=0.0, art=0.0}
         @logs' d/1.0
   |]

----------

hasFlowered :: Multiset Agent -> Bool
hasFlowered mix = (sumM dg . select isEPlant) mix < 2604

hasGerminated :: Multiset Agent -> Bool
hasGerminated mix=  (sumM dg . select isSeed) mix < 1000
