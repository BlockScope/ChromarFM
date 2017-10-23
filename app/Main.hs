{-# LANGUAGE TransformListComp #-}

import           Chromar
import           Control.Lens                           hiding (at)
import           Control.Monad
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Data.List
import           GHC.Exts                               (groupWith, the)
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Plant
import qualified System.Random                          as R

outDir = "out/fmliteExps8h"

main =
    goPlot
        10
        [ carbon
        , leafMass
        , starch
        , cc
        , grC
        , grD
        , rootMass
        , leaf1Mass
        , leaf5Mass
        , leaf10Mass
        , leaf12Mass
        ]
        [0 .. 1300]
        outDir

goPlot nreps obss tss outDir = do
    rgen <- R.getStdGen
    let obssF = map gen obss
    let obsNms = map name obss
    let nObs = length obss
    let trajs = runTT rgen nreps hasFlowered mdLite
    let tobsss = map (flip applyObs obssF) trajs
    let stobsss = map (tsample tss) tobsss
    mapM_ (plotObs obsNms stobsss outDir) [0 .. (nObs - 1)]
    mapM_ (writeAvgObs obsNms stobsss outDir) [0 .. (nObs - 1)]
    print $ avgLastTime stobsss

writeAvgObs nms tobsss outDir i = writeFile fout (unlines stpoints)
  where
    bOutDir = outDir ++ "/" ++ "text"
    fout = bOutDir ++ "/" ++ (nms !! i) ++ ".txt"
    avgTj = avgTraj i tobsss
    stpoints = map (\(t, v) -> show t ++ " "  ++ show v) avgTj

--- plot ith observable
plotObs nms tobsss outDir i = renderableToFile def fout chart
  where
    bOutDir = outDir ++ "/" ++ "plots"
    fout = bOutDir ++ "/" ++ (nms !! i) ++ ".png"
    avgTj = avgTraj i tobsss
    lines = map (mkLine i) tobsss ++ [mkSolidLine avgTj]

    layout = layout_plots .~ lines
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_x_axis . laxis_title .~ "time (h)"
           $ layout_x_axis . laxis_title_style . font_size .~ 20.0
           $ layout_y_axis . laxis_title .~ (nms !! i)
           $ layout_y_axis . laxis_title_style . font_size .~ 20.0
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def

    chart = toRenderable layout

mkLine :: Int -> [(Time, [Obs])] -> Plot Time Obs
mkLine i tobss =
    toPlot
        (plot_lines_values .~ [mkXYPairs i tobss] $ plot_lines_style . line_color .~
         (blue `withOpacity` 0.2) $
         plot_lines_style .
         line_width .~
         3.0 $
         def)

mkSolidLine :: [(Time, Obs)] -> Plot Time Obs
mkSolidLine tobss =
    toPlot
        (plot_lines_values .~ [tobss] $ plot_lines_style . line_color .~
         opaque red $
         plot_lines_style .
         line_width .~
         3.0 $
         def)

mkXYPairs :: Int -> [(Time, [Obs])] -> [(Time, Obs)]
mkXYPairs i tobss =
    [ (t, obss !! i)
    | (t, obss) <- tobss ]

avgT :: Time -> [Fluent Obs] -> Obs
avgT t fs = avg [at f t | f <- fs]

avgTraj i tobsss =
    [ (t, avgT t fluents)
    | t <- [1 .. te] ]
  where
    tobsssi = map (mkXYPairs i) tobsss :: [[(Time, Obs)]]
    fluents = map flookup tobsssi
    te = 1300

avgHour tobss = [(fromIntegral (the (map floor t)), avg obs) | (t, obs) <- tobss,
                 then group by (floor t) using groupWith]

runTT
    :: (Eq a)
    => R.StdGen -> Int -> (Multiset a -> Bool) -> Model a -> [[State a]]
runTT gen n fb md
    | n == 0 = []
    | otherwise = traj : runTT rg2 (n - 1) fb md
  where
    (rg1, rg2) = R.split gen
    traj = takeWhile (fb . getM) (simulate rg1 (rules md) (initState md))

runUntil
    :: (Ord a, Show a)
    => Model a -> (Multiset a -> Bool) -> FilePath -> [Observable a] -> IO ()
runUntil Model {rules = rs
               ,initState = s} fb fn obss = do
    rgen <- R.getStdGen
    let traj = takeWhile (fb . getM) (simulate rgen rs s)
    writeObs fn obss traj

tsample :: [Time] -> [(Time, a)] -> [(Time, a)]
tsample _ [] = []
tsample [] _ = []
tsample (ts1:tss) [(t1, v1)] = [(ts1, v1)]
tsample ts@(ts1:tss) tv@((t1, v1):(t2, v2):tvs)
    | ts1 < t1 = (ts1, v1) : tsample tss tv
    | ts1 >= t1 && ts1 < t2 = (ts1, v1) : tsample tss ((t2, v2) : tvs)
    | ts1 >= t2 = tsample ts tvs

avgLastTime :: [[(Time, a)]] -> Time
avgLastTime tobss = avg $ map (fst . last) tobss

mainDistr :: IO ()
mainDistr = do
    gen <- R.getStdGen
    let tend = (365 * 60 * 24)
    let traj =
            takeWhile
                (\s -> getT s < tend)
                (simulate gen (rules md) (initState md))
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
    mapM_ print rms
    print "----------"
    mapM_ print gts
    print "----------"
    mapM_ print fts
    print "----------"
    mapM_ print ss
