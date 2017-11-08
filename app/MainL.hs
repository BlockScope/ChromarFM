{-# LANGUAGE TransformListComp #-}
module Main where

import           Agent
import           Chromar
import           Control.Lens                           hiding (at)
import           Control.Monad
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Data.List
import           Env
import           GHC.Exts                               (groupWith, the)
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           PlantL
import qualified System.Random                          as R
import           Utils


mkSt'' :: Env -> Int -> Multiset Agent
mkSt'' e n = ms seeds
  where
    seeds = [Seed {mass=1.6e-5,
                   attr=Attrs {ind=i, psi=psim e, fi=frepr e},
                   dg=0.0,
                   art=0.0}
            | i <- [1..n]]

mdSimpl e =
    Model
    { rules =
        [ dev
        , devep
        , germ
        , transp
        , devfp
        , transfp
        ]
    , initState = mkSt'' e 20
    }

seedDev = Observable { gen = sumM dg . select isSeed,
                       name = "seedDev" }

nSeeds = Observable { gen=countM . select isSeed, name ="nSeeds" }
nPlants = Observable { gen = countM . select isEPlant, name="nPlants"}
nFPlants = Observable { gen = countM . select isFPlant, name = "nFPlants"}

writeOut nms fout tobss = writeFile fout (unlines rows)
  where
    header = "time" ++ "," ++ intercalate "," nms
    rows = header : [show t ++ ","  ++ showr obs | (t, obs) <- tobss]
    showr obss = intercalate "," $ map show obss

gop obss tss fout md = do
  rgen <- R.getStdGen
  let obssF = map gen obss
  let obsNms = map name obss
  let nObs = length obss
  let traj = simulate rgen (rules md) (initState md)
  let tobss = (flip applyObs obssF) traj
  let stobss = (tsample tss) tobss
  writeOut obsNms fout stobss

main = do
    print "running..."
    gop
        [nSeeds, nPlants, nFPlants]
        [0,24 .. 10*365 * 24]
        "out/lifeExpsVal/outNs.txt"
        (mdSimpl
             (Env
             { psim = 0.0
             , frepr = 0.598
             }))
