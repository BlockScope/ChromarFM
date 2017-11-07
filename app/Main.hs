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

mkSt' :: Multiset Agent
mkSt' =
    ms
        [ Plant
          { thrt = 0.0
          , attr =
              Attrs
              { ind = 1
              , psi = 0.0
              , fi = 0.598
              }
          , dg = 0.0
          , wct = 0.0
          }
        ]

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
        , devp
        , devep
        , eme
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
        , leaf1Mass
        , leaf5Mass
        , leaf10Mass
        , leaf12Mass
        , nL
        , tRDem
        ]
        [0 .. 365*24*5]
        outDir
        mdLite
        hasFlowered
  where
    outDir = "out/fmliteExpsVal"

main = mainLite
