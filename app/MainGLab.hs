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
         Cell{attr=atr, c = initC * ra, s=si}, VAxis{nv=2}]
  where
    cotMass = cotArea / slaCot
    fR = rdem 0.0 thrmFinal
    ra = 2*cotArea*cos (10/180*pi)
    si = initS * initC * ra
    atr = Attrs{ind=1, psi=0.0, fi=0.598}

mkSt'' :: Multiset Agent
mkSt'' = ms [Plant{attr=atr, thrt=1.0, dg=0.0, wct=0.0}]
  where
    atr = Attrs{ind=1, psi=0.0, fi=0.598}

mdLiteGreenlab =
  Model { rules =
             [eme,
              devp,
              growth,
              growthRepr,
              growthRepr',
              assim,
              leafCr,
              starchConv,
              starchFlow',
              maintRes,
              lmaintRes,
              inMaintRes,
              frMaintRes,
              rootGrowth,
              rootGrowthRepr,
              rootMaint,
              leafTransl',
              rootTransl',
              devep,
              transp,
              devfp,
              vGrowth,
              vGrowthFruit,
              lGrowth,
              lGrowthFruit,
              llGrowth,
              inodeGrowth,
              fruitGrowth,
              leafD,
              leafD',
              starchConvRepr]
          ,initState= mkSt'' }

main' = do
    let dur = 7440
        outDir = "out/greenlabExps"
    print "running"    
    goPlot
        5
        [ carbon
        , leafMass
        , starch
        , rootMass
        , nL
        , plantDev
        , tLDem
        , tRDem
        , nVLeaves
        , isRStage
        , reprDev
        , nLAxis
        , plantDem
        , tInDem
        , tFDem
        , tLLDem
        , apFruit
        , tLLDem
        , tLeafDem
        , nLatLeaves
        , maxN
        , ngLAxis
        , tDelayObs 20
        , tDelayObs 30
        , qd
        , mMALeaves
        , mMAInodes
        , mMAFruits
        , mLAInodes
        , mLAFruits
        , mLALeaves
        , nMAFruits
        , nLAFruits
        , plantMass
        , mRosLeaves
        , rArea
        , totalFMass  
        ] 
        [0 .. dur]
        outDir
        mdLiteGreenlab
        (\s -> getT s < dur)

mainRun = do
    let outDir = "out/greenlabExps"
    print "running"
    runTW
        mdLiteGreenlab
        1848
        "out/greenlabExps/text/out.txt"
        [ carbon
        , leafMass
        , starch
        , rootMass
        , nL
        , plantDev
        , tLDem
        , tRDem
        , nVLeaves
        , isRStage
        , reprDev
        , nLAxis
        , plantDem
        , tInDem
        , tFDem
        , tLLDem
        , apFruit
        , tLLDem
        , tLeafDem
        , nLatLeaves
        , maxN
        , ngLAxis
        , tDelayObs 20
        , tDelayObs 30
        , qd
        , mMALeaves
        , mMAInodes
        , mMAFruits
        , mLAInodes
        , mLAFruits
        , mLALeaves
        , nMAFruits
        , nLAFruits
        , plantMass
        , mRosLeaves
        , rArea
        , totalFMass  
        ] 

main = mainRun
