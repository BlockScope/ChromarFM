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
import           Plant
import qualified System.Random                          as R
import           Utils


mkSt'' :: Env -> Multiset Agent
mkSt'' e = ms ([System{germTimes = [], flowerTimes=[], ssTimes=[], rosMass=[]}] ++ seeds)
  where
    seeds = [Seed {mass=1.6e-5, attr=Attrs {ind=i, psi=psim e, fi=frepr e}, dg=0.0, art=0.0} | i <- [1..10]]

md e =
    Model
    { rules =
        [ dev
        , devep
        , emeGermSimpl
        , transp
        , devfp
        , transfp
        ]
    , initState = mkSt'' e
    }

mdSeed =
    Model
    { rules = [dev]
    , initState =
        ms
            [ Seed
              { mass = 1.6e-5
              , attr =
                  Attrs
                  { ind = 1
                  , psi = 2.5
                  , fi = 0.598
                  }
              , dg = 0.0
              , art = 0.0
              }
            ]
    }

seedDev = Observable { gen = sumM dg . select isSeed,
                       name = "seedDev" }

report e fout gts fts ss rms = appendFile fout (unlines rows)
  where
    tag = "f" ++ show (frepr e) ++ "_" ++ "d" ++ show (psim e)
    out (gt, ft, ss, rm, tag) =
        show gt ++
        " " ++ show ft ++ " " ++ show ss ++ " " ++ show rm ++ " " ++ tag
    header =
        "germT" ++
        " " ++
        "flowerT" ++ " " ++ "ssetT" ++ " " ++ "rosMass" ++ " " ++ "params"
    rows =
        header :
        (map
             out
             (zip5
                  (reverse gts)
                  (reverse fts)
                  (reverse ss)
                  (reverse rms)
                  (repeat tag)))

mainDistr :: FilePath -> (Double, Double) -> IO ()
mainDistr fout (pm, fr) = do
    print (pm, fr)
    let e = Env { psim = pm, frepr = fr}
        mdE = md e
    gen <- R.getStdGen
    let tend = (365 * 50 * 24)
    let traj =
            takeWhile
                (\s -> getT s < tend)
                (simulate gen (rules mdE) (initState mdE))
    let lState = getM (last traj)
    let rms =
            head
                [ rm
                | (System {rosMass = rm}, _) <- lState ]
    let (gts, fts, ss) =
            head
                [ (gt, ft, s)
                | (System {germTimes = gt
                          ,flowerTimes = ft
                          ,ssTimes = s}, _) <- lState ]
    let gts =
            head
                [ gt
                | (System {germTimes = gt}, _) <- lState ]
    report e fout gts fts ss rms

mainLife :: IO ()
mainLife = do
  let pms = [0.0, 2.5]
      frs = [0.598, 0.737]
  forM_ [(p, f) | p <- pms, f <- frs] (mainDistr "out/lifeExpsVal/lifecycles.txt")

writeOut nms fout tobss = writeFile fout (unlines rows)
  where
    header = "time" ++ " " ++ intercalate " " nms
    rows = header : [show t ++ " "  ++ showr obs | (t, obs) <- tobss]
    showr obss = intercalate " " $ map show obss

mainSeed = do
  print "hello"
  goPlot 5 [seedDev] [1..2*365*24] "out/lifeExpsVal" mdSeed hasGerminated

nSeeds = Observable { gen=countM . select isSeed, name ="nSeeds" }
nPlants = Observable { gen = countM . select isEPlant, name="nPlants"}
nFPlants = Observable { gen = countM . select isFPlant, name = "nFPlants"}

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
        (md
             (Env
             { psim = 0.0
             , frepr = 0.598
             }))

