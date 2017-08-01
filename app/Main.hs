import Plant
import Chromar
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens hiding (at)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart
import Control.Monad
import Data.List
import qualified System.Random as R

fplot =
    [ "out/leafMass.png"
    , "out/rArea.png"
    , "out/carbon.png"
    , "out/nl.png"
    , "out/rootMass.png"
    ]

fout = "out/out.txt"


--main = runT md (365*1*24) [leafMass, eplantD]
--main = runUntil md hasFlowered fout [leafMass, rArea, carbon, nL, rootMass, plantD, eplantD]
main = goPlot
--main = mainDistr


mainDistr :: IO ()
mainDistr = do
  gen <- R.getStdGen
  let tend = (365*60*24)
  let traj = takeWhile (\s -> getT s < tend) (simulate gen (rules md) (initState md))
  let lState = getM (last traj)
  let rms = head [rm | (System{rosMass=rm}, _) <- lState]
  let (gts, fts, ss) = head [(gt, ft, s) | (System{germTimes=gt, flowerTimes=ft, ssTimes=s}, _) <- lState]
  let gts = head [gt | (System{germTimes=gt}, _) <- lState]
  mapM_ print rms
  print "----------"
  mapM_ print gts
  print "----------"
  mapM_ print fts
  print "----------"
  mapM_ print ss

goPlot = do
    rgen <- R.getStdGen
    let obssF = map gen [leafMass, eplantD]
    let trajs = runTT rgen 22 hasFlowered md
    let tobsss = map ((flip applyObs) obssF) (drop 2 trajs)
    mapM_ (plotObs fplot tobsss) [0, 1]
    
avgT :: Time -> [Fluent Obs] -> Obs
avgT t fs = avg [at f t | f <- fs]

avgTraj i tobsss =
    [ (fromIntegral t, avgT (fromIntegral t) fluents)
    | t <- [0 .. tend] ]
  where
    tobsssi = map (mkXYPairs i) tobsss :: [[(Time, Obs)]]
    fluents = map flookup tobsssi
    
runTT
    :: (Eq a)
    => R.StdGen -> Int -> (Multiset a -> Bool) -> Model a -> [[State a]]
runTT gen n fb md
    | n == 0 = []
    | otherwise = traj : (runTT rg2 (n - 1) fb md)
  where
    (rg1, rg2) = R.split gen
    traj =
        takeWhile (\s -> fb (getM s)) (simulate rg1 (rules md) (initState md))

--- plot ith observable
plotObs fn tobsss i = renderableToFile def (fn !! i) chart
  where
    avgTj = avgTraj i tobsss
    lines = (map (mkLine i) tobsss) ++ [mkSolidLine avgTj]
    
    layout = layout_plots .~ lines
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 18.0  
           $ layout_x_axis . laxis_title .~ "time (h)"
           $ layout_x_axis . laxis_title_style . font_size .~ 20.0  
           $ layout_y_axis . laxis_title .~ "Rosette weight (g)"
           $ layout_y_axis . laxis_title_style . font_size .~ 20.0
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def
           
    chart = toRenderable layout

mkXYPairs :: Int -> [(Time, [Obs])] -> [(Time, Obs)]
mkXYPairs i tobss =
    [ (t, obss !! i)
    | (t, obss) <- tobss ]

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


