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
import System.Environment

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

mdLiteGreenlab =
  Model { rules =
             [growth,
              assim,
              leafCr,
              starchConv,
              starchFlow,
              maintRes,
              rootGrowth,
              rootMaint,
              leafTransl,
              rootTransl,
              devep,
              transp,
              devfp,
              vGrowth,
              vGrowthFruit,
              lGrowth,
              lGrowthFruit]
          ,initState= mkSt' }

mainLite :: FilePath -> Time -> IO ()
mainLite outDir dur=
    goPlot
        5
        [ carbon
        , leafMass
        , starch
        , rootMass
        , nL
        , plantDev
        , thrtt
        , tLDem
        , tRDem
        , nVLeaves
        ]
        [0 .. dur]
        outDir
        mdLiteGreenlab
        (\s -> getT s < dur)

main = do
    let outDir = "out/greenlabExps"
        tstart = 0
        tend = 1000
    print "running"
    runTW
        mdLiteGreenlab
        2000
        "out/greenlabExps/text/out.txt"
        [ carbon
        , leafMass
        , starch
        , rootMass
        , nL
        , plantDev
        , thrtt
        , tLDem
        , tRDem
        , nVLeaves
        , isRStage
        , reprDev
        , nLAxis
        , gLAxis 1
        , gLAxis 3
        , gLAxis 5
        , gLAxis 8
        , gLAxis 10
        , tDelayObs
        , plantDem
        , tInDem
        , tFDem
        , tLLDem
        , apFruit
        , tLLDem
        , tLeafDem
        , nLatLeaves  
        ] 
