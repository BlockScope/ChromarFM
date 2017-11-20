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
import Params

outDir = "out/fmliteExpsVal"

mkSt' :: Multiset Agent
mkSt' = ms
        [EPlant{sdeg=calcSDeg si time, thrt=0.0, attr=atr, dg=0.0, wct=0.0},
         Leaf{attr=atr, i = 1, ta = 0.0, m = cotArea/slaCot, a = cotArea},
         Leaf{attr=atr, i = 2, ta = 0.0, m = cotArea/slaCot, a = cotArea},
         Root {attr=atr, m = pr * fR * (seedInput / (pr*fR + 2)) },
         Cell{attr=atr, c = initC * ra, s=si}]
  where
    cotMass = cotArea / slaCot
    fR = rdem 0.0 thrmFinal
    ra = 2*cotArea*cos (10/180*pi)
    si = initS * initC * ra
    atr = Attrs{ind=1, psi=0.0, fi=0.598}
      
mdLite =
    Model
    { rules =
        [ growth
        , assim
        , leafCr
        , starchConv
        , starchFlow
        , maintRes
        , rootGrowth
        , rootMaint
        , leafTransl
        , rootTransl
        , devep
        ]
    , initState = mkSt'
    }

mainLite =
    goPlot
        5
        [ carbon
        , leafMass
        , starch
        , rootMass
        , nL
        ]
        [0 .. 70*24]
        outDir
        mdLite
        (\s -> getT s < 69*24)

main = mainLite
