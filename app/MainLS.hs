{-# LANGUAGE OverloadedStrings #-}

module Main where

import Agent
import Chromar
import Control.Monad
import Data.List
import Data.Random.Normal
import Env
import PlantL
import qualified System.Random as R
import Data.Maybe
import Types
import qualified Data.ByteString.Lazy as B
import Data.Csv

mkSt'' e n psis = ms seeds
  where
    seeds = [Seed {mass=1.6e-5,
                   attr=Attrs {ind=i, psi=p, fi=frepr e},
                   dg=0.0,
                   art=0.0}
            | (i, p) <- zip [1..n] psis]

mdSimpl e psis =
    Model
    { rules =
        [ dev
        , devep
        , germ
        , transp
        , devfp
        , transfp
        ]
    , initState = mkSt'' e 1 psis
    }

nseeds = Observable { gen=countM . select isSeed, name ="nseeds" }
nplants = Observable { gen = countM . select isEPlant, name="nplants"}
nfplants = Observable { gen = countM . select isFPlant, name = "nfplants"}

writeOut nms fout tobss = writeFile fout (unlines rows)
  where
    header = "time" ++ "," ++ intercalate "," nms
    rows = header : [show t ++ ","  ++ showr obs | (t, obs) <- tobss]
    showr obss = intercalate "," $ map show obss

writeEvents :: FilePath -> [Event] -> IO ()
writeEvents fout events = writeFile fout (unlines rows)
  where
    rows = undefined

gop obss tss fout md = do
  rgen <- R.getStdGen
  let obssF = map gen obss
  let obsNms = map name obss
  let nObs = length obss
  let traj = simulate rgen (rules md) (initState md)
  let tobss = (flip applyObs obssF) traj
  let stobss = (tsample tss) tobss
  writeOut obsNms fout stobss

mkEvent :: Rxn Agent -> Maybe (Int, LifeEvent)
mkEvent Rxn{lhs=[(Seed{attr=atr}, 1)], rhs=[(EPlant{}, 1)]} = Just (ind atr, Germ)
mkEvent Rxn{lhs=[(EPlant{attr=atr}, 1)], rhs=[(FPlant{}, 1)]} = Just (ind atr, Flower)
mkEvent Rxn{lhs=[(FPlant{attr=atr}, 1)], rhs=[(Seed{}, 1)]} = Just (ind atr, SeedSet)
mkEvent _ = Nothing

collectEvent :: State Agent -> Maybe Event
collectEvent (State _ r t _) = do
  (i, eventT) <- mkEvent r
  return $ Event { timeE=t, pid=i, typeE=eventT}

gopEvents fout tend md = do
    rgen <- R.getStdGen
    let traj =
            takeWhile
                (\s -> getT s < tend)
                (simulate rgen (rules md) (initState md))
    let events = catMaybes (map collectEvent traj)
    B.writeFile fout (encodeDefaultOrderedByName events)

tsample :: [Time] -> [(Time, a)] -> [(Time, a)]
tsample _ [] = []
tsample [] _ = []
tsample (ts1:tss) [(t1, v1)] = [(ts1, v1)]
tsample ts@(ts1:tss) tv@((t1, v1):(t2, v2):tvs)
    | ts1 < t1 = (ts1, v1) : tsample tss tv
    | ts1 >= t1 && ts1 < t2 = (ts1, v1) : tsample tss ((t2, v2) : tvs)
    | ts1 >= t2 = tsample ts tvs

main = do
    psis <- normalsIO
    print "running..."
    gopEvents
        "out/lifeExpsVal/outNsL.txt"
        (5*365*24)
        (mdSimpl
             (Env
             { psim = 0.0
             , frepr = 0.737
             }) (take 1 psis) )
    -- gop
    --     [nseeds, nplants, nfplants]
    --     [0,24 .. 20*365 * 24]
    --     "out/lifeExpsVal/outNsL.txt"
    --     (mdSimpl
    --          (Env
    --          { psim = 0.0
    --          , frepr = 0.737
    --          }) (take 100 psis) )
    -- print "done with L.."
    -- gop
    --     [nseeds, nplants, nfplants]
    --     [0,24 .. 60*365 * 24]
    --     "out/lifeExpsHal/outNsH.txt"
    --     (mdSimpl
    --          (Env
    --          { psim = 2.5
    --          , frepr = 0.737
    --          }) (take 100 psis) )
    -- print "done with H.."
