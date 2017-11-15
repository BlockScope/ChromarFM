{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import           Control.Applicative
import           Control.Lens                           hiding (at)
import qualified Data.ByteString.Lazy                   as BL
import           Data.Colour
import           Data.Colour.Names
import           Data.Csv
import           Data.Default.Class
import           Data.Fixed
import           Data.List
import qualified Data.Map                               as M
import qualified Data.Vector                            as V
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           System.Environment

getDayYear :: Double -> Double
getDayYear time = fromIntegral dayYear
  where
    hourYear = mod' time (365*24)
    dayYear = truncate (hourYear / 24.0)

type Time = Double

type TSeries a = [(Time, a)]

data Sample = Sample
    { time     :: Double
    , nseeds   :: Double
    , nplants  :: Double
    , nfplants :: Double
    } deriving (Show)

data Out
    = PlotO
    | WriteO
    deriving (Show)

instance FromNamedRecord Sample where
    parseNamedRecord r =
        Sample <$> r .: "time" <*> r .: "nseeds" <*> r .: "nplants" <*>
        r .: "nfplants"

avg l =
    let (t, n) = foldl' (\(b, c) a -> (a + b, c + 1)) (0, 0) l
    in (realToFrac t / realToFrac n)

dropYrs :: Int -> TSeries Double -> TSeries Double
dropYrs n ts = dropWhile (\(t, _) -> t < yrHours) ts
  where
    yrHours = fromIntegral (n * 365*24)

extractTSeries :: (Sample -> a) -> [Sample] -> TSeries a
extractTSeries ext ps = [(time p, ext p) | p <- ps]

avgYear :: TSeries Double -> TSeries Double
avgYear ts = M.toList avgy
  where
    toYear ts = [(getDayYear h, el) | (h, el) <- ts]
    toN ts = [(d, 1) | (d, el) <- ts]
    m = M.fromListWith (+) (toYear ts)
    n = M.fromListWith (+) (toN . toYear $ ts)
    avgy = M.unionWith (/) m n

mkLine :: String -> Colour Double -> [(Time, Double)] -> Plot Time Double
mkLine s c tvals =
    toPlot
        (  plot_lines_values .~ [tvals]
         $ plot_lines_style . line_color .~ (c `withOpacity` 0.9)
         $ plot_lines_style . line_width .~ 2.0
         $ plot_lines_title .~s
         $ def
        )

plotLines plots fout = renderableToFile def fout chart
  where
    layout = layout_plots .~ plots
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_x_axis . laxis_title .~ "time (h)"
           $ layout_x_axis . laxis_title_style . font_size .~ 20.0
           $ layout_y_axis . laxis_title_style . font_size .~ 20.0
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def

    chart = toRenderable layout

plotOut fout nms tss = plotLines lines fout
  where
    colours = [blue, red, magenta, green, gray, brown]
    lines = [mkLine n c ts | (c, n, ts) <- zip3 colours nms tss]

{-
Write multiple timeseries assuming that their time indexes are the same
precondition is not checked
-}
writeOut fout nms (ts1, ts2, ts3) = writeFile fout (unlines rows)
  where
    catc t v1 v2 v3 = intercalate "," (map show [t, v1, v2, v3])
    header = "time" ++ "," ++  (intercalate "," nms)
    rows = header : [catc t v1 v2 v3 | ((t, v1), (_, v2), (_, v3)) <- zip3 ts1 ts2 ts3]

goAvgYearPlot fout ps =
    plotOut fout ["nseeds", "nplants", "nfplants"] [tseeds, tplants, tfplants]
  where
    tseeds = avgYear (extractTSeries nseeds ps)
    tplants = avgYear (extractTSeries nplants ps)
    tfplants = avgYear (extractTSeries nfplants ps)

goAvgYearWrite fout ps =
    writeOut fout ["nseeds", "nplants", "nfplants"] (tseeds, tplants, tfplants)
  where
    tseeds = avgYear (dropYrs 15 . extractTSeries nseeds $ ps)
    tplants = avgYear (dropYrs 15 . extractTSeries nplants $ ps)
    tfplants = avgYear (dropYrs 15 . extractTSeries nfplants $ ps)

parseOutMode :: String -> Out
parseOutMode "Plot" = PlotO
parseOutMode "Write" = WriteO
parseOutMode _ = error "can't parse output mode"

main = do
  args <- getArgs
  let fin = args !! 0
      fout = args !! 1
      outMode = parseOutMode (args !! 2)
  csvData <- BL.readFile fin
  case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> case outMode of
          PlotO -> do { p <- goAvgYearPlot fout (V.toList v); return () }
          WriteO -> goAvgYearWrite fout (V.toList v)
