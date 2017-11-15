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
import           Data.Random.Normal
import           Env
import           GHC.Exts                               (groupWith, the)
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Plant
import qualified System.Random                          as R
import           Data.Set.Monad     (Set)

mkSt'' e n= ms seeds
  where
    seeds = [Seed {mass=1.6e-5,
                   attr=Attrs {ind=i, psi=psim e, fi=frepr e},
                   dg=0.0,
                   art=0.0}
            | i <- [1..n]] ++ [System{germTimes=[], flowerTimes=[], ssTimes=[], rosMass=[]}]

-- mdSimpl e =
--     Model
--     { rules =
--         [ dev
--         , devep
--         , germ
--         , transp
--         , devfp
--         , transfp
--         ]
--     , initState = mkSt'' e 20
--     }

mdFM e =
    Model
    { rules =
        [ dev
        , growth
        , assim
        , starchConv
        , starchFlow
        , leafCr
        , maintRes
        , rootGrowth
        , rootMaint
        , leafTransl
        , devp
        , devep
        , emeGerm
        , leafD
        , leafD'
        , transp
        , devfp
        , transfp
        ]
    , initState = mkSt'' e 1
    }

nseeds = Observable { gen=countM . select isSeed, name ="nseeds" }
nplants = Observable { gen = countM . select isEPlant, name="nplants"}
nfplants = Observable { gen = countM . select isFPlant, name = "nfplants"}

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

tsample :: [Time] -> [(Time, a)] -> [(Time, a)]
tsample _ [] = []
tsample [] _ = []
tsample (ts1:tss) [(t1, v1)] = [(ts1, v1)]
tsample ts@(ts1:tss) tv@((t1, v1):(t2, v2):tvs)
    | ts1 < t1 = (ts1, v1) : tsample tss tv
    | ts1 >= t1 && ts1 < t2 = (ts1, v1) : tsample tss ((t2, v2) : tvs)
    | ts1 >= t2 = tsample ts tvs

main = do
    print "running..."
    gop
        [nseeds, nplants, nfplants]
        [0,24 .. 5*365 * 24]
        "out/lifeExpsVal/outNsLFM.txt"
        (mdFM
             (Env
             { psim = 0.0
             , frepr = 0.737
             }))
