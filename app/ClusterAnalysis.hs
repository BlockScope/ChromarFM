{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ClusterAnalysis where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import GHC.Generics (Generic)
import Data.Text.Encoding
import Data.Char
import AnalysisEvents
import qualified Data.Map as M
import Types
import Data.List
import GHC.Exts
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens hiding (at, (.>), (|>), elements, assign)
import Data.Time.Calendar.MonthDay
import Data.Default.Class
import Data.List.Split
import Data.List

mkVeg :: [Lifecycle] -> [(Double, Double)]
mkVeg lfs = (0,0): (365, 365): (map (\lf -> (germD lf, vegSLenD lf)) lfs)

avgVeg :: [Lifecycle] -> (Double, Double)
avgVeg lfs = (avg $ germDs lfs, avg $ vegSLensD lfs)

mkRepr :: [Lifecycle] -> [(Double, Double)]
mkRepr lfs = (0,0): (365, 365): (map (\lf -> (flowerD lf, reprSLenD lf)) lfs)

avgRepr :: [Lifecycle] -> (Double, Double)
avgRepr lfs = (avg $ flowerDs lfs, avg $ reprSLensD lfs)

dormSLenD = dormSLen .> (/24)

dormSLensD = map dormSLenD

data TVal = TVal
  { t  :: !Double
  , val :: !Double
  } deriving (Generic, Show)

instance FromRecord TVal

instance ToRecord TVal

add :: [TVal] -> [TVal] -> [TVal]
add tvs1 tvs2 = [TVal{t=t tv1, val=val tv1 + val tv2} | (tv1, tv2) <- zip tvs1 tvs2]

toTuples :: [TVal] -> [(Double, Double)]
toTuples tvals = [(t tv, val tv) | tv <- tvals]

readSimOut :: FilePath -> IO [TVal]
readSimOut fin = do
  csvData <- BL.readFile fin
  case decodeWith
         (defaultDecodeOptions {decDelimiter = fromIntegral $ ord ' '})
         NoHeader
         csvData of
    Left err -> error err
    Right v -> return $ V.toList v

data GVals = GVals
  { rosM :: [TVal]
  , inflM :: [TVal]
  , fruitM :: [TVal]
  }

instance Show GVals where
  show g = ""

data GCluster = GCluster
  { cid :: Int
  , fmass :: Double
  , growth :: GVals
  , lfs :: [Lifecycle]
  } deriving (Show)

mkCluster :: Int -> [Lifecycle] -> GCluster
mkCluster i lfs =
  GCluster
  { cid = i
  , fmass = 0
  , growth=GVals{rosM=[], inflM=[], fruitM=[]}
  , lfs = lfs
  }

type Mass = Double

assign :: [(GCluster, [Double])] -> Lifecycle -> GCluster
assign cs lf =
    head .> fst $
    sortWith
        snd
        [ (g, dist lf ctr)
        | (g, ctr) <- cs ]
  where
    dist lf c = euclDist c (extractL lf)

getCentroids :: [GCluster] -> [(GCluster, [Double])]
getCentroids cs = zip cs (map extract cs)

assignCs :: [GCluster] -> [Lifecycle] -> [GCluster]
assignCs cs ls = map (assign ctrs) ls
  where
    ctrs = getCentroids cs

breadthW :: Mass -> Double
breadthW m
    | m < 0.2 = 0.0
    | otherwise = 1.0
    
logistic :: (Double, Double) -> Double -> Double
logistic (k, x0) x = 1.0 / (1 + exp (-k*(x - x0)))

getMassLineage :: [GCluster] -> (Mass -> Double) -> [Lifecycle] -> Mass
getMassLineage cs f lfs = sum (zipWith (*) mss bss)
  where
    mss = map fmass (assignCs cs lfs)
    bss = scanl (*) 1.0 (map f mss)

getLineage :: [GCluster] -> [Lifecycle] -> [Lifecycle]
getLineage cs lfs = map fst (takeWhile (\(lf, b) -> b > 0.0) (zip lfs bss))
  where
    mss = map (fmass .> breadthW) (assignCs cs lfs)
    bss = scanl (*) 1.0 mss
    
lineages ess = M.map (sortWith timeE .> dropYrsE 15 .> getLfs) ess

euclDist :: [Double] -> [Double] -> Double
euclDist xs ys = sum $ zipWith (\x y -> (x - y) ** 2) xs ys

extract :: GCluster -> [Double]
extract c =
  [ avg $ map (getLen .> (/ 24)) lives
  , avg $ germDs lives
  , avg $ vegSLensD lives
  , avg $ flowerDs lives
  , avg $ reprSLensD lives
  ]
  where
    lives = lfs c

extractL :: Lifecycle -> [Double]
extractL lf =
  [getLen .> (/ 24) $ lf, germD lf, vegSLenD lf, flowerD lf, reprSLenD lf]

addGrowth :: GVals -> GCluster -> GCluster
addGrowth gvals c = c {fmass = fm, growth = gvals}
  where
    fm = val $ last (fruitM gvals)

toTupleG :: GVals -> [(Double, Double, Double, Double)]
toTupleG gvals =
  [ (t rm * 24, val rm, val im, val fm)
  | (rm, im, fm) <- zip3 (rosM gvals) (inflM gvals) (fruitM gvals)
  ]

fromTupleG :: [(Double, Double, Double, Double)] -> GVals
fromTupleG tvals = GVals { rosM = [TVal{t=t, val=rm} | (t, rm, _, _) <- tvals],
                           inflM = [TVal{t=t, val=im} | (t, _, im, _) <- tvals],
                           fruitM = [TVal{t=t, val=fm} | (t, _, _, fm) <- tvals]}

writeCluster :: FilePath -> GCluster -> IO ()
writeCluster fp c = BL.writeFile fp (encode $ toTupleG (growth c))

readCluster :: FilePath -> IO GVals
readCluster fp = do
  csvData <- BL.readFile fp
  case decode NoHeader csvData of
    Left err -> error err
    Right v -> return $ fromTupleG (V.toList v)

mkLineLeg :: [String] -> [Colour Double] -> String -> String -> [[(Double, Double)]] -> Layout Double Double
mkLineLeg ds cs ytitle xtitle tvalss = layout
  where
    plot d c tvals =
        toPlot
            (  plot_lines_values .~ [tvals]
             $ plot_lines_style . line_color .~ (c `withOpacity` 0.7)
             $ plot_lines_style . line_width .~ 2.0
             $ plot_lines_title .~ d
             $ def)
    layout = layout_plots .~ [plot d c vals | (d, c, vals) <- zip3 ds cs tvalss]
           $ layout_title  .~ ""
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_x_axis . laxis_title_style . font_size .~ 16.0
           $ layout_x_axis . laxis_title .~ xtitle
           $ layout_y_axis . laxis_title_style . font_size .~ 16.0
           $ layout_y_axis . laxis_title .~ ytitle
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def
           
mkLine'' :: [String] -> [Colour Double] -> String -> String -> [[(Double, Double)]] -> Layout Double Double
mkLine'' ds cs ytitle xtitle tvalss = layout
  where
    plot d c tvals =
        toPlot
            (  plot_lines_values .~ [tvals]
             $ plot_lines_style . line_color .~ (c `withOpacity` 0.7)
             $ plot_lines_style . line_width .~ 2.0
             $ def)
    layout = layout_plots .~ [plot d c vals | (d, c, vals) <- zip3 ds cs tvalss]
           $ layout_title  .~ ""
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_x_axis . laxis_title_style . font_size .~ 16.0
           $ layout_x_axis . laxis_title .~ xtitle
           $ layout_y_axis . laxis_title_style . font_size .~ 16.0
           $ layout_y_axis . laxis_title .~ ytitle
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def
           
mkGrowthLines' gvals =
  mkLine''
    ["rosette", "inflorescence", "fruit"]
    [blue, green, red]
    "mass (g)"
    "time (days)"
    [toTuples $ rosM gvals, toTuples $ inflM gvals, toTuples $ fruitM gvals]

mkGrowthLinesLeg gvals =
  mkLineLeg
    ["rosette", "inflorescence", "fruit"]
    [blue, green, red]
    "mass (g)"
    "time (days)"
    [toTuples $ rosM gvals, toTuples $ inflM gvals, toTuples $ fruitM gvals]


toDays :: [TVal] -> [TVal]
toDays tvals = [TVal {t = t / 24, val = v} | TVal {t = t, val = v} <- tvals]

toDayG :: GVals -> GVals
toDayG GVals {rosM = rm, inflM = im, fruitM = fm} =
  GVals {rosM = toDays rm, inflM = toDays im, fruitM = toDays fm}


plotHistsGrid' fout x hists = renderableToFile foptions fout $ fillBackground def $ chart
  where
    histsG = map layoutToGrid hists
    fullGrid = aboveN (map besideN (chunksOf x histsG))

    chart = gridToRenderable fullGrid

    foptions = fo_format .~ PDF $ fo_size .~ (800, 500) $ def

mkGVals :: FilePath -> IO GVals
mkGVals fp = do
  mRosLeaves <- readSimOut "../out/greenlabExps/text/mRosLeaves.txt"
  mMALeaves <- readSimOut "../out/greenlabExps/text/mMALeaves.txt"
  mLALeaves <- readSimOut "../out/greenlabExps/text/mLALeaves.txt"
  mMAFruits <- readSimOut "../out/greenlabExps/text/mMAFruits.txt"
  mLAFruits <- readSimOut "../out/greenlabExps/text/mLAFruits.txt"
  return $
    GVals
    { rosM = toDays mRosLeaves
    , inflM = toDays (add mMALeaves mLALeaves)
    , fruitM = toDays (add mMAFruits mLAFruits)
    }

quartile :: (Ord a) => Int -> [a] -> a
quartile k xs = (sort xs) !! (round q)
  where
    nxs = length xs 
    q = fromIntegral nxs * ((fromIntegral k) / 4)
    
quartM :: [Double] -> Double
quartM xs = avg [quartile 1 xs, quartile 2 xs, quartile 3 xs]

monthTicks :: AxisData Double -> AxisData Double
monthTicks ad = axis_labels .~ [mticks] $ ad
  where
    ticksDays = [0, 91, 183, 274, 365] :: [Double]
    dticks = [(t, show t) | t <- ticksDays]
    mticks = [(15, "Jan"), (105, "Apr"), (197, "Jul"), (288, "Oct"), (350, "Dec")]
    
getMonth :: Double -> Int    
getMonth d = m
  where
    (m, _) = dayOfYearToMonthAndDay False (round d)
    
monthString :: Int -> String
monthString 1 = "Jan"
monthString 2 = "Feb"
monthString 3 = "Mar"
monthString 4 = "Apr"
monthString 5 = "May"
monthString 6 = "Jun"
monthString 7 = "Jul"
monthString 8 = "Aug"
monthString 9 = "Sep"
monthString 10 = "Oct"
monthString 11 = "Nov"
monthString 12 = "Dec"
monthString _ = error "only 12 months"

monthLabels = map (getMonth .> monthString)

clLabels 1.0 = "1"
clLabels 2.0 = "2"
clLabels 3.0 = "3"
clLabels 4.0 = "4"
clLabels _ = ""
    
params = la_labelf .~ monthLabels $ la_nLabels .~ 5 $ la_nTicks .~ 5 $ def

paramsY = la_labelf .~ (map clLabels) $ la_nLabels .~ 4 $ la_nTicks .~ 4 $ def

mkYrPoints :: [String] -> [Colour Double] -> String -> String -> [[(Double, Double)]] -> Layout Double Double
mkYrPoints ds cs xtitle ytitle valss = layout
  where
    plot d c vals = toPlot (plot_points_style .~ filledCircles 5 (c `withOpacity` 0.4)
              $ plot_points_values .~ vals
              $ plot_points_title .~ d
              $ def )   
    layout = layout_plots .~ [plot d c vals | (d, c, vals) <- zip3 ds cs valss]
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_x_axis . laxis_title_style . font_size .~ 16.0
           $ layout_x_axis . laxis_title .~ xtitle
           $ layout_y_axis . laxis_title_style . font_size .~ 16.0
           $ layout_y_axis . laxis_title .~ ytitle
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ layout_x_axis . laxis_generate .~ (scaledAxis params (1.0, 365.0))
           $ layout_y_axis . laxis_generate .~ (scaledAxis paramsY (0.5, 4.5))
           $ def

mkYrLines :: [String] -> [Colour Double] -> String -> String -> [[(Double, Double)]] -> Layout Double Double
mkYrLines ds cs xtitle ytitle valss = layout
  where
    plot d c tvals =
        toPlot
            (  plot_lines_values .~ [tvals]
             $ plot_lines_style . line_color .~ (c `withOpacity` 0.7)
             $ plot_lines_style . line_width .~ 2.0
             $ plot_lines_title .~ d  
             $ def)
    layout = layout_plots .~ [plot d c vals | (d, c, vals) <- zip3 ds cs valss]
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_x_axis . laxis_title_style . font_size .~ 16.0
           $ layout_x_axis . laxis_title .~ xtitle
           $ layout_y_axis . laxis_title_style . font_size .~ 16.0
           $ layout_y_axis . laxis_title .~ ytitle
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ layout_x_axis . laxis_generate .~ (scaledAxis params (1.0, 365.0))
           $ layout_y_axis . laxis_generate .~ (scaledAxis paramsY (0.5, 4.5))
           $ def

mkYrCloud :: [String] -> [Colour Double] -> String -> String -> [[(Double, Double)]] -> Layout Double Double
mkYrCloud ds cs xtitle ytitle valss = layout
  where
    plot d c vals = toPlot (plot_points_style .~ filledCircles 5 (c `withOpacity` 0.4)
              $ plot_points_values .~ vals
              $ plot_points_title .~ d
              $ def )   
    layout = layout_plots .~ [plot d c vals | (d, c, vals) <- zip3 ds cs valss]
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 15.0
           $ layout_x_axis . laxis_title_style . font_size .~ 16.0
           $ layout_x_axis . laxis_title .~ xtitle
           $ layout_y_axis . laxis_title_style . font_size .~ 16.0
           $ layout_y_axis . laxis_title .~ ytitle
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ layout_x_axis . laxis_generate .~ (scaledAxis params (1.0, 365.0))
           $ def

mkLPoints ::
     ([Lifecycle] -> [Double]) -> ([Lifecycle], Double) -> [(Double, Double)]
mkLPoints f (ls, i) =
  [(quartile 1 xs, i), (quartile 2 xs, i), (quartile 3 xs, i)]
  where
    xs = f ls

mkLLinePs c = [vegL, reprL]
  where
    i = fromIntegral (cid c)
    ls = lfs c
    vegL = [(avg $ germDs ls, i), (avg $ flowerDs ls, i)]
    reprL = [(avg $ flowerDs ls, i), (avg $ ssetDs ls, i)]
    
mkLPointsC :: (GCluster -> [Double]) -> GCluster -> [(Double, Double)]
mkLPointsC f c = [(quartile 1 xs, i), (quartile 2 xs, i), (quartile 3 xs, i)]
  where
    xs = f c
    i = fromIntegral (cid c)

germDsC :: GCluster -> [Double]
germDsC = lfs .> germDs

flowerDsC :: GCluster -> [Double]
flowerDsC = lfs .> flowerDs

ssetDsC :: GCluster -> [Double]
ssetDsC = lfs .> ssetDs

plotClusters cls =
  mkYrPoints
    ["germ", "flower", "seed"]
    [brown, green, red]
    ""
    ""
    [germs, flowers, sdrops]
  where
    germs = concatMap (mkLPointsC germDsC) cls
    flowers = concatMap (mkLPointsC flowerDsC) cls
    sdrops = concatMap (mkLPointsC ssetDsC) cls

plotClusterLs cs = mkLine' cls "" "" lns
  where
    lns = concatMap mkLLinePs cs
    cls = concat (repeat [blue, red])

plotGrowthPeriods cls =
  [ mkYrCloud ["veg", "repr"] cls "" "length (days)" gp
  | (cls, gp) <- zip colours grPeriods
  ]
  where
    colours =
      [[blue, lightblue], [red, pink], [green, lightgreen], [black, gray]]
    grPeriods = map (\lfs -> [mkVeg lfs, mkRepr lfs]) (map lfs cls)

succsOf :: Int -> [Int] -> [Int]
succsOf k [] = []
succsOf k [n] = []
succsOf k (n:n':ns) 
    | k == n = n':succsOf k (n':ns)
    | otherwise = succsOf k (n':ns)

update :: Int -> M.Map Int Int -> M.Map Int Int
update k = M.adjustWithKey (\k v -> v + 1) k

freqs :: [Int] -> M.Map Int Int
freqs ns = foldl' (flip update) (M.fromList [(1, 0), (2, 0), (3, 0), (4, 0)]) ns

freqAnalysis cs events = map ratioN succsFreqs
  where
    livesI = lineages events
    liveTypes = M.foldr (++) [] (M.map ((assignCs cs) .> (map cid)) livesI)
    cids = map cid cs
    succsFreqs =
      map (M.toList .> (map snd)) [freqs (succsOf cid liveTypes) | cid <- cids]
