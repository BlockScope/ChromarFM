module Env where

import Chromar.Fluent

dayTemp = 22.0 :: Double
nightTemp = 22.0 :: Double
baseTemp = 3.0 :: Double

dayTemp' = dayTemp - baseTemp
nightTemp' = nightTemp - baseTemp

light = between 6 18 (constant True) (constant False)
day = repeatEvery 24 light

par = 120.0

d = 12.0

temp = when day (constant dayTemp') `orElse` (constant nightTemp')

calcCTemp :: Time -> Double
calcCTemp t = ((dayHours * dayTemp') + (nightHours * nightTemp') + todayThr) / 24.0
  where
    tr = round t :: Int
    (days, hours) = quotRem tr 24
    dayHours = fromIntegral $ days * 12
    nightHours = fromIntegral $ days * 12
    todayThr = sum [at temp (fromIntegral h) | h <- [1 .. hours]]

thr = mkFluent calcCTemp

co2 = 42.0
