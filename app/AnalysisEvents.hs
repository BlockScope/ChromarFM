{-# LANGUAGE OverloadedStrings #-}

module AnalysisEvents where

import Types
import Control.Applicative
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

data Lifecycle = Lifecycle
    { germT :: Double
    , flowerT :: Double
    , ssetT :: Double
    }

instance Show Lifecycle where
    show Lifecycle {germT = gt
                   ,flowerT = ft
                   ,ssetT = st} = show gt ++ " " ++ show ft ++ " " ++ show st

flookupMDef ::
    a -> M.Map Time a -> Fluent a
flookupMDef def tvals =
    mkFluent (\t -> fromMaybe def $ fmap snd (M.lookupLE t tvals))

type TSeries a = [(Time, a)]

getDayYear :: Double -> Double
getDayYear time = fromIntegral dayYear
  where
    hourYear = mod' time (365*24)
    dayYear = truncate (hourYear / 24.0)

avg l =
    let (t, n) = foldl' (\(b, c) a -> (a + b, c + 1)) (0, 0) l
    in (realToFrac t / realToFrac n)

dropYrs :: Int -> TSeries Int -> TSeries Int
dropYrs n ts = dropWhile (\(t, _) -> t < yrHours) ts
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
    toYear ts = [(getDayYear h, fromIntegral el) | (h, el) <- ts]
    toN ts = [(d, 1) | (d, el) <- ts]
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
    header = "time" ++ "," ++  (intercalate "," nms)
    rows = header : [catc t v1 v2 v3 | ((t, v1), (_, v2), (_, v3)) <- zip3 ts1 ts2 ts3]

goAvgYearWrite fout ps =
    writeOut fout ["nseeds", "nplants", "nfplants"] (tseeds, tplants, tfplants)
  where
    tss = [0,24 .. 60*365 * 24]
    tseeds = avgYear (dropYrs 15 . extractTSeries nSeeds tss $ ps)
    tplants = avgYear (dropYrs 15 . extractTSeries nPlants tss $ ps)
    tfplants = avgYear (dropYrs 15 . extractTSeries nFPlants tss $ ps)

getLifecycles :: [Event] -> [Lifecycle]
getLifecycles es = map length (chunksOf 3 es)
  where
    length [Event{timeE=t, typeE=Germ},
            Event{timeE=t1, typeE=Flower},
            Event{timeE=t2, typeE=SeedSet}] =
      Lifecycle { germT = t, flowerT=t1, ssetT = t2}
    length _ = Lifecycle {germT=0, flowerT=0, ssetT=0}

getLifecycleDistr :: [Event] -> [Lifecycle]
getLifecycleDistr es = foldr (++) [] lifesId
  where
    idGrouped= M.fromListWith (++) [(pid e, [e]) | e <- es]
    sortedEvents = M.map (sortWith timeE) idGrouped
    lifesId = M.map getLifecycles sortedEvents

getLengths :: [Lifecycle] -> [Double]
getLengths ls =
    [ len
    | life <- ls
    , let len = ssetT life - germT life
    , len > 0 ]

main = do
  args <- getArgs
  let fin = args !! 0
      fout = args !! 1
  csvData <- BL.readFile fin
  case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> mapM_ (putStrLn . show) $ getLengths (getLifecycleDistr (V.toList v))
