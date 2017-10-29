module PlotUtils where

import           Control.Lens                           hiding (at)
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import qualified Data.Map.Strict                        as Map
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as TI
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo


type Time = Double

mkLine :: String -> Colour Double -> [(Time, Double)] -> Plot Time Double
mkLine s c tvals =
    toPlot
        (  plot_lines_values .~ [tvals]
         $ plot_lines_style . line_color .~ (c `withOpacity` 0.9)
         $ plot_lines_style . line_width .~ 2.0
         $ plot_lines_title .~s
         $ def
        )

plotLines plots = renderableToFile def fout chart
  where
    fout = "plot.png"
    layout = layout_plots .~ plots
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 18.0
           $ layout_x_axis . laxis_title .~ "time (h)"
           $ layout_x_axis . laxis_title_style . font_size .~ 20.0
           $ layout_y_axis . laxis_title_style . font_size .~ 20.0
           $ layout_legend .~ Just (legend_label_style . font_size .~ 16.0 $ def)
           $ def

    chart = toRenderable layout

go nms tvalss = plotLines plots
  where
    colours = [blue, red, magenta, green, gray, brown]

    plots = [mkLine s c vals | (s, c, vals) <- zip3 nms colours tvalss]








