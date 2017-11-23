{-# LANGUAGE OverloadedStrings #-}

module AnalysisEvents where

import Types
import Control.Applicative
import Control.Monad
import Control.Lens hiding (at, (.>), (|>))
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
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Environment
import Chromar.Fluent
import Data.Maybe
import Data.List.Split
import GHC.Exts
import System.FilePath.Posix

{- sometimes it's nicer to use these operators when doing data transformations
as it looks more natural to write M.map (sortWith timeE .> dropYrsE 15 .> getLifecycels)
and write the operations in the order that they happen (from left to right)
 -}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

(|>) :: a -> (a -> b) -> b
x |> f = apply x f

apply :: a -> (a -> b) -> b
apply x f = f x

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

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

isNSSet Event{typeE=Flower} = True
isNSSet Event{typeE=Germ} = True
isNSSet _ = False

{-
  we should add a phantom type to keep track of the
  unit for time + type parameter for time
-}
data Lifecycle = Lifecycle
    { pidL :: Int
    , pssetT :: Double
    , germT :: Double
    , flowerT :: Double
    , ssetT :: Double
    } deriving Show

toWeek :: Lifecycle -> Lifecycle
toWeek Lifecycle {pidL = p
                 ,pssetT = pst
                 ,germT = gt
                 ,flowerT = ft
                 ,ssetT = st} =
    Lifecycle
    { pidL = p
    , pssetT = fromIntegral $ truncate (pst / 7)
    , germT = fromIntegral $ truncate (gt / 7)
    , flowerT = fromIntegral $ truncate (ft / 7)
    , ssetT = fromIntegral $ truncate (st / 7)
    }

eqTiming :: Lifecycle -> Lifecycle -> Bool
eqTiming Lifecycle {germT = gt
                   ,pssetT = pst
                   ,flowerT = ft
                   ,ssetT = st} Lifecycle {pssetT = pst'
                                          ,germT = gt'
                                          ,flowerT = ft'
                                          ,ssetT = st'} =
    (gt == gt') && (ft == ft') && (st == st') && (pst == pst')

compCounts :: (a, Int) -> (a, Int) -> Ordering
compCounts (_, n) (_, m)
  | n > m = GT
  | n == m = EQ
  | n < m = LT           

flookupMDef ::
    a -> M.Map Time a -> Fluent a
flookupMDef def tvals =
    mkFluent (\t -> fromMaybe def $ fmap snd (M.lookupLE t tvals))

type TSeries a = [(Time, a)]

mkHist :: (RealFrac a) => Colour Double -> [a] -> Plot a Int
mkHist c vals =
    histToPlot
        (  plot_hist_fill_style . fill_color .~ (c `withOpacity` 0.1) $
           plot_hist_values .~ vals $ def)

{- plots histograms on top of each other -}
plotHists fout xtitle plots = renderableToFile foptions fout chart
  where
    layout = layout_plots .~ plots
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_x_axis . laxis_title_style . font_size .~ 20.0
           $ layout_x_axis . laxis_title .~ xtitle
           $ layout_y_axis . laxis_title_style . font_size .~ 20.0
           $ layout_y_axis . laxis_title .~ "counts"
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def

    chart = toRenderable layout

    foptions = fo_size .~ (500,200) $ def

mkHist' :: [Colour Double] -> String -> String -> [[Double]] -> Layout Double Int
mkHist' cs xtitle title valss = layout
  where
    plot c vals =
        histToPlot
            (plot_hist_fill_style . fill_color .~ (c `withOpacity` 0.1) $
             plot_hist_values .~ vals $
             def :: PlotHist Double Int)
    layout = layout_plots .~ [plot c vals | (c, vals) <- zip cs valss]
           $ layout_title  .~ title
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_x_axis . laxis_title_style . font_size .~ 20.0
           $ layout_x_axis . laxis_title .~ xtitle
           $ layout_y_axis . laxis_title_style . font_size .~ 20.0
           $ layout_y_axis . laxis_title .~ "counts"
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def

{- plots histograms arranged on a grid -}
plotHistsGrid fout x hists = renderableToFile foptions fout $ fillBackground def $ chart
  where
    histsG = map layoutToGrid hists
    fullGrid = aboveN (map besideN (chunksOf x histsG))

    chart = gridToRenderable fullGrid

    foptions = fo_size .~ (900,500) $ def

{- plots histograms arranged on a grid -}
plotHistsGridR x hists = fillBackground def $ chart
  where
    histsG = map layoutToGrid hists
    fullGrid = aboveN (map besideN (chunksOf x histsG))

    chart = gridToRenderable fullGrid

getDayYear :: Double -> Double
getDayYear time = fromIntegral dayYear
  where
    hourYear = mod' time (365*24)
    dayYear = truncate (hourYear / 24.0)

avg l =
    let (t, n) = foldl' (\(b, c) a -> (a + b, c + 1)) (0, 0) l
    in (realToFrac t / realToFrac n)

median l = sort l !! (length l `quot` 2)

dropYrs :: Int -> TSeries Int -> TSeries Int
dropYrs n ts = dropWhile (\(t, _) -> t < yrHours) ts
  where
    yrHours = fromIntegral (n * 365*24)

dropYrsE :: Int -> [Event] -> [Event]
dropYrsE n es = dropWhile isNSSet (dropWhile (\e -> timeE e < yrHours) es)
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
    header = "time" ++ "," ++ intercalate "," nms
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

mkLifecycle :: [Event] -> [Event] -> Maybe Lifecycle
mkLifecycle [Event{timeE=t, pid=i, typeE=SeedSet},
             Event{timeE=t1, typeE=Germ},
             Event{timeE=t2, typeE=Flower}] (Event{timeE=t3, typeE=SeedSet}:ess2) =
  Just Lifecycle {pidL =i, pssetT=t, germT=t1, flowerT=t2, ssetT=t3}
mkLifecycle _ _ = Nothing

collectLFs :: [[Event]] -> [Maybe Lifecycle]
collectLFs [] = []
collectLFs [es] = []
collectLFs (es1:es2:ess) = mkLifecycle es1 es2 : collectLFs (es2:ess)

getLfs :: [Event] -> [Lifecycle]
getLfs es = catMaybes (collectLFs (chunksOf 3 es))

groupById :: [Event] -> M.Map Int [Event]
groupById es =
    M.fromListWith
        (++)
        [ (pid e, [e])
        | e <- es ]

getLfDistr :: [Event] -> [Lifecycle]
getLfDistr es =
    M.foldr (++) [] (M.map (sortWith timeE .> dropYrsE 15 .> getLfs) es')
  where
    es' = groupById es

getLens :: [Lifecycle] -> [Double]
getLens ls =
    [ len
    | life <- ls
    , let len = ssetT life - pssetT life
    , len > 0 ]

writeLens :: FilePath -> [Double] -> IO ()
writeLens fout ls = writeFile fout (unlines $ map show ls)

sortLFs :: Double -> Double -> [Lifecycle] -> ([Lifecycle], [Lifecycle])
sortLFs t1 t2 ls = (livesF cond1, livesF cond2)
  where
    cond1 t1 t2 = abs t1 < abs t2
    cond2 t1 t2 = abs t1 >= abs t2
    livesF cond =
        [ life
        | life <- ls
        , let len = ssetT life - pssetT life
        , let ct1 = len - t1
        , let ct2 = len - t2
        , cond ct1 ct2 ]

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

showEnv' (p, f) = "d" ++ show p ++ "_" ++ "r" ++ show f

mkFNamePsis bfout loc e =
    bfout ++ codeName loc ++ "/outEvents_psis" ++ showEnv' e ++ ".txt"

mkFName bfout loc e =
    bfout ++ codeName loc ++ "/outEventsH" ++ "_" ++ showEnv' e ++ ".txt"

mkFOut bfout loc e =
    bfout ++
    codeName loc ++ "/outEvents" ++ "_" ++ showEnv' e ++ "AvgYear.txt"

mkFOutPlot bfout loc e nm =
    bfout ++ codeName loc ++ "/plots/" ++ nm ++ showEnv' e ++ ".png"

doTimings :: FilePath -> IO ()
doTimings bfout = mapM_ doTiming fnames
  where
    locs = [Norwich, Halle, Oulu, Valencia]
    envs =
        [ (p, f)
        | p <- [0.0, 2.5]
        , f <- [0.598, 0.737] ]
    fnames =
        [ (mkFName bfout loc (p, f), mkFOut bfout loc (p, f))
        | loc <- locs
        , (p, f) <- envs ]
    doTiming (fin, fout) = readEvents fin >>= (\es -> goAvgYearWrite fout es)

doLengthsHist (fin, fout) = do
  lives <- fmap getLfDistr (readEvents fin)
  let lens = map (/24) (getLens lives)
  plotHists fout "days" [mkHist blue lens]

doNLivesHist (fin, fout) = do
  es <- fmap groupById (readEvents fin)
  let lives = M.map
              (fromIntegral . length . getLfs . dropYrsE 15 . sortWith timeE)
              es :: M.Map Int Double
  plotHists fout "# of lifecycles (45 years)" [mkHist red (M.elems lives)]

{- do histogram for lifecycle lengths
   and # of lifecycles per location per genotype
-}
doHistograms :: FilePath -> IO ()
doHistograms bfout = do
    mapM_ doLengthsHist fnames
    mapM_ doNLivesHist fnames'
  where
    locs = [Norwich, Halle, Oulu, Valencia]
    envs =
        [ (p, f)
        | p <- [0.0, 2.5]
        , f <- [0.598, 0.737] ]
    fnames =
        [ (mkFName bfout loc (p, f), mkFOutPlot bfout loc (p, f) "lengths")
        | loc <- locs
        , (p, f) <- envs ]
    fnames' =
        [ (mkFName bfout loc (p, f), mkFOutPlot bfout loc (p, f) "nLives")
        | loc <- locs
        , (p, f) <- envs ]

vegSLen :: [Lifecycle] -> [Time]
vegSLen lives = [flowerT l - germT l | l <- lives]

reprSLen :: [Lifecycle] -> [Time]
reprSLen lives = [ssetT l - flowerT l | l <- lives]

dormSLen lives = [germT l - pssetT l | l <- lives]

vegF :: FilePath -> IO ()
vegF fp = do
    print fp
    lives <- fmap getLfDistr (readEvents fp)
    print $ avg (map (getDayYear . germT) lives)
    print $ (avg $ vegSLen lives) / 24

reprF :: FilePath -> IO ()
reprF fp = do
    print fp
    lives <- fmap getLfDistr (readEvents fp)
    print $ avg (map (getDayYear . flowerT) lives)
    print $ (avg $ reprSLen lives) / 24

doFiles :: FilePath -> (FilePath -> IO ()) -> IO ()
doFiles bfout f = mapM_ f fnames
  where
    locs = [Valencia, Oulu, Halle, Norwich]
    envs = [(p, f) | p <- [0.0, 2.5], f <- [0.598, 0.737]]
    fnames = [mkFName bfout loc (p, f) | loc <- locs, (p, f) <- envs]

doPsis :: FilePath -> IO ()
doPsis bfout = mapM_ plotPsis fnames
  where
    locs = [Valencia, Oulu, Halle, Norwich]
    envs =
        [ (p, f)
        | p <- [0.0, 2.5]
        , f <- [0.598, 0.737] ]
    fnames =
        [ (mkFNamePsis bfout loc (p, f), mkFOutPlot bfout loc (p, f) "psisH")
        | loc <- locs
        , (p, f) <- envs ]
    plotPsis (fin, fout) =
        readPsis fin >>=
        (\mpsi -> plotHists fout "psi" [mkHist green (M.elems mpsi)])
