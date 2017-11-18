{-# LANGUAGE OverloadedStrings #-}

module AnalysisEvents where

import Types
import Control.Applicative
import Control.Monad
import Control.Lens hiding (at)
import qualified Data.ByteString.Lazy as BL
import Data.Colour
import Data.Colour.Names
import Data.Csv
import Data.Default.Class
import Data.Fixed
import Data.List
import qualified Data.Map as M
import qualified Data.Vector as V
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import System.Environment
import Chromar.Fluent
import Data.Maybe
import Data.List.Split
import GHC.Exts
import System.FilePath.Posix

data Location
    = Norwich
    | Halle
    | Valencia
    | Oulu
    deriving (Show)

codeName :: Location -> String
codeName Norwich = "Nor"
codeName Halle = "Hal"
codeName Oulu = "Oul"
codeName Valencia = "Val"

fLoc :: Location -> String
fLoc loc = fname (codeName loc)
  where
    fname nm = "../out/lifeExps" ++ nm ++ "/outEventsLL.txt" :: String

isNGerm Event{typeE=Flower} = True
isNGerm Event{typeE=SeedSet} = True
isNGerm _ = False

{- we should add a phantom type to keep track of the time + type parameter for time -}
data Lifecycle = Lifecycle
    { pidL :: Int
    , germT :: Double
    , flowerT :: Double
    , ssetT :: Double
    }

instance Show Lifecycle where
    show Lifecycle { pidL = p
                   , germT = gt
                   ,flowerT = ft
                   ,ssetT = st} =
        show gt ++ " " ++ show ft ++ " " ++ show st

toDay :: Lifecycle -> Lifecycle
toDay Lifecycle {pidL=p
                ,germT = gt
                ,flowerT = ft
                ,ssetT = st} =
    Lifecycle
    { pidL = p
    , germT = getDayYear gt
    , flowerT = getDayYear ft
    , ssetT = getDayYear st
    }

toWeek :: Lifecycle -> Lifecycle
toWeek Lifecycle {pidL=p
                ,germT = gt
                ,flowerT = ft
                ,ssetT = st} =
    Lifecycle
    { pidL = p
    , germT = fromIntegral $ truncate (gt / 7)
    , flowerT = fromIntegral $ truncate (ft / 7)
    , ssetT = fromIntegral $ truncate (st / 7)
    }

eqTiming :: Lifecycle -> Lifecycle -> Bool
eqTiming Lifecycle {germT = gt
                   ,flowerT = ft
                   ,ssetT = st} Lifecycle {germT = gt'
                                          ,flowerT = ft'
                                          ,ssetT = st'} =
    (gt == gt') && (ft == ft') && (st == st')

{- most occured lifecycles with weekly granularity -}
mostOccured :: [Lifecycle] -> Lifecycle
mostOccured ls =
    Lifecycle
    { pidL = 0
    , germT = mocc germT
    , flowerT = mocc flowerT
    , ssetT = mocc ssetT
    }
  where
    lsw = map (toWeek . toDay) ls
    mocc f = (head . fst) $ 
        (maximumBy
            compCounts
            [ (g, length g)
            | g <- group (sort $ map f lsw) ])

compCounts :: (a, Int) -> (a, Int) -> Ordering
compCounts (_, n) (_, m)
  | n > m = GT
  | n == m = EQ
  | n < m = LT           

toDayL = map toDay

toWeekL = map toWeek

flookupMDef ::
    a -> M.Map Time a -> Fluent a
flookupMDef def tvals =
    mkFluent (\t -> fromMaybe def $ fmap snd (M.lookupLE t tvals))

type TSeries a = [(Time, a)]

mkHistogram :: (RealFrac a) => Colour Double -> [a] -> Plot a Int
mkHistogram c vals =
    histToPlot
        (  plot_hist_fill_style . fill_color .~ (c `withOpacity` 0.1) $
           plot_hist_values .~ vals $ def)

plotLines plots = renderableToFile def fout chart
  where
    fout = "plot.png"
    layout = layout_plots .~ plots
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_x_axis . laxis_title_style . font_size .~ 20.0
           $ layout_y_axis . laxis_title_style . font_size .~ 20.0
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def

    chart = toRenderable layout

getDayYear :: Double -> Double
getDayYear time = fromIntegral dayYear
  where
    hourYear = mod' time (365*24)
    dayYear = truncate (hourYear / 24.0)

avg l =
    let (t, n) = foldl' (\(b, c) a -> (a + b, c + 1)) (0, 0) l
    in (realToFrac t / realToFrac n)

median l = (sort l) !! (length l `quot` 2)

dropYrs :: Int -> TSeries Int -> TSeries Int
dropYrs n ts = dropWhile (\(t, _) -> t < yrHours) ts
  where
    yrHours = fromIntegral (n * 365*24)

dropYrsE :: Int -> [Event] -> [Event]
dropYrsE n es = dropWhile isNGerm (dropWhile (\e -> timeE e < yrHours) es)
  where
    yrHours = fromIntegral (n * 365*24)

extractTSeries :: (Event -> a) -> [Time] -> [Event] -> TSeries a
extractTSeries ext tss ps = [(t, at f t) | t <- tss]
  where
    n = ext $ head ps
    f = flookupMDef n (M.fromList [(timeE p, ext p) | p <- ps])

avgYear :: TSeries Int -> TSeries Double
avgYear ts = M.toList avgy
  where
    toYear ts =
        [ (getDayYear h, fromIntegral el)
        | (h, el) <- ts ]
    toN ts =
        [ (d, 1)
        | (d, el) <- ts ]
    m = M.fromListWith (+) (toYear ts)
    n = M.fromListWith (+) (toN . toYear $ ts)
    avgy = M.unionWith (/) m n

{-
Write multiple timeseries assuming that their time indexes are the same
precondition is not checked
-}
writeOut fout nms (ts1, ts2, ts3) = writeFile fout (unlines rows)
  where
    catc t v1 v2 v3 = intercalate "," (map show [t, v1, v2, v3])
    header = "time" ++ "," ++ (intercalate "," nms)
    rows =
        header :
        [ catc t v1 v2 v3
        | ((t, v1), (_, v2), (_, v3)) <- zip3 ts1 ts2 ts3 ]

goAvgYearWrite fout ps =
    writeOut fout ["nseeds", "nplants", "nfplants"] (tseeds, tplants, tfplants)
  where
    tss = [0,24 .. 60 * 365 * 24]
    tseeds = avgYear (dropYrs 15 . extractTSeries nSeeds tss $ ps)
    tplants = avgYear (dropYrs 15 . extractTSeries nPlants tss $ ps)
    tfplants = avgYear (dropYrs 15 . extractTSeries nFPlants tss $ ps)

getLifecycles :: [Event] -> [Lifecycle]
getLifecycles es = map length (chunksOf 3 es)
  where
    length [Event{timeE=t, pid=i, typeE=Germ},
            Event{timeE=t1, typeE=Flower},
            Event{timeE=t2, typeE=SeedSet}] =
      Lifecycle { pidL =i, germT = t, flowerT=t1, ssetT = t2}
    length _ = Lifecycle {pidL=0, germT=0, flowerT=0, ssetT=0}


groupById :: [Event] -> M.Map Int [Event]
groupById es =
    M.fromListWith
        (++)
        [ (pid e, [e])
        | e <- es ]

getLifecycleDistr :: [Event] -> [Lifecycle]
getLifecycleDistr es = M.foldr (++) [] lifesId
  where
    sortedEvents = M.map (sortWith timeE) (groupById es)
    lifesId = M.map getLifecycles sortedEvents

getLengths :: [Lifecycle] -> [Double]
getLengths ls =
    [ len
    | life <- ls
    , let len = ssetT life - germT life
    , len > 0 ]

writeLengths :: FilePath -> [Double] -> IO ()
writeLengths fout ls = writeFile fout (unlines $ map show ls)

doAnalysis :: FilePath -> [Event] -> IO ()
doAnalysis fp es = do
    let lifeLengths = getLengths (getLifecycleDistr es)
        avgYearFout = (dropExtension fp) ++ "AvgYear.txt"
        lengthsFout = (dropExtension fp) ++ "Lengths.txt"
    goAvgYearWrite avgYearFout es
    writeLengths lengthsFout lifeLengths

sortLFs :: (Lifecycle -> a) -> Double -> Double -> [Lifecycle] -> ([a], [a])
sortLFs f t1 t2 ls = (livesF cond1, livesF cond2)
  where
    cond1 t1 t2 = abs t1 < abs t2
    cond2 t1 t2 = abs t1 >= abs t2
    livesF cond =
        [ f life
        | life <- ls
        , let len = ssetT life - germT life
        , let ct1 = len - t1
        , let ct2 = len - t2
        , cond ct1 ct2 ]

getAvgLifecycle :: [Lifecycle] -> Lifecycle
getAvgLifecycle ls =
    Lifecycle
    { pidL = 0
    , germT = avg (map germT ls)
    , flowerT = avg (map flowerT ls)
    , ssetT = avg (map ssetT ls)
    }

getMedianLifecycle :: [Lifecycle] -> Lifecycle
getMedianLifecycle ls =
    Lifecycle
    { pidL = 0
    , germT = median (map germT ls)
    , flowerT = median (map flowerT ls)
    , ssetT = median (map ssetT ls)
    }

bimodalTest :: [Event] -> Time -> Time -> (Lifecycle, Lifecycle)
bimodalTest es t1 t2 =
    (toDay $ getMedianLifecycle ls1, toDay $ getMedianLifecycle ls2)
  where
    ls = getLifecycleDistr es
    (ls1, ls2) = sortLFs id t1 t2 ls

bimodalTest' :: [Event] -> Time -> Time -> M.Map Int Double -> [(Double, Int)]
bimodalTest' es t1 t2 psis = rows ++ rows1
  where
    ls = filter (\l -> pidL l > 0) (getLifecycleDistr es)
    (ls1, ls2) = sortLFs pidL t1 t2 ls
    rows =
        [ (psis M.! i, 1)
        | i <- ls1 ]
    rows1 =
        [ (psis M.! i, 2)
        | i <- ls2 ]

writePsisDistr :: FilePath -> [(Double, Int)] -> IO ()
writePsisDistr fout psis =
    writeFile fout $
    unlines
        [ show p ++ " " ++ show i
        | (p, i) <- psis ]

getPsis :: [Double] -> M.Map Int Double
getPsis psis =
    M.fromList
        [ (i, p)
        | (i, p) <- zip [1 .. length psis] psis ]

multIndex :: [Int] -> M.Map Int a -> [a]
multIndex is m = [m M.! i | i <- is]

readEvents :: FilePath -> IO [Event]
readEvents fin = do
  csvData <- BL.readFile fin
  case decodeByName csvData of
    Left err -> error err
    Right (_, v) -> return $ V.toList v

readPsis :: FilePath -> IO (M.Map Int Double)
readPsis fin = do
  psisS <- readFile fin
  return $ getPsis (map read (lines psisS))
  
main = do
    args <- getArgs
    let fin = args !! 0
        pf = args !! 1
    csvData <- BL.readFile fin
    psis <- readPsis pf
    events <- readEvents fin
    doAnalysis fin events


-- print $ bimodalTest events (50 * 24) (220 * 24)
-- writePsisDistr
--     "../out/lifeExpsVal/psisModes.txt"
--     (bimodalTest' events (50 * 24) (220 * 24) psis)

-- doAnalysis fin (V.toList v)
-- print $ bimodalTest (V.toList v) (50*24) (220*24)          
