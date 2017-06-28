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


--main = runT md tend [leafMass, rArea, carbon, nL, rootMass, plantD]
main = runUntil md hasFlowered fout [leafMass, rArea, carbon, nL, rootMass, plantD]
--main = goPlot

goPlot = do
    rgen <- R.getStdGen
    let obssF = map gen [leafMass, rArea, carbon, nL, rootMass]
    let trajs = runTT rgen 10 tend md
    let tobsss = map ((flip applyObs) obssF) trajs
    mapM_ (plotObs fplot tobsss) [0, 1, 2, 3, 4]

avg l =
    let (t, n) = foldl' (\(b, c) a -> (a + b, c + 1)) (0, 0) l
    in (realToFrac (t) / realToFrac (n))

avgT :: Time -> [Fluent Obs] -> Obs
avgT t fs = avg [at f t | f <- fs]

avgTraj i tobsss =
    [ (t, avgT t fluents)
    | t <- [0 .. tend] ]
  where
    tobsssi = map (mkXYPairs i) tobsss :: [[(Time, Obs)]]
    fluents = map flookup tobsssi
    
runTT
    :: (Eq a)
    => R.StdGen -> Int -> Time -> Model a -> [[State a]]
runTT gen n tend md
    | n == 0 = []
    | otherwise = traj : (runTT rg2 (n - 1) tend md)
  where
    (rg1, rg2) = R.split gen
    traj =
        takeWhile (\s -> getT s < tend) (simulate rg1 (rules md) (initState md))

--- plot ith observable
plotObs fn tobsss i = renderableToFile def (fn !! i) chart
  where
    avgTj = avgTraj i tobsss
    lines = (map (mkLine i) tobsss) ++ [mkSolidLine avgTj]
    layout = layout_plots .~ lines $ def
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
         2.0 $
         def)

mkSolidLine :: [(Time, Obs)] -> Plot Time Obs
mkSolidLine tobss =
    toPlot
        (plot_lines_values .~ [tobss] $ plot_lines_style . line_color .~
         opaque red $
         plot_lines_style .
         line_width .~
         2.0 $
         def)


