module Env where

import Chromar.Fluent

fi = 0.5741
fu = 0


dayTemp = 22.0 :: Double
nightTemp = 22.0 :: Double
baseTemp = 3.0 :: Double

dayTemp' = dayTemp - baseTemp
nightTemp' = nightTemp - baseTemp

photo' = constant 12
d = 12.0

light = between 6 18 (constant True) (constant False)
day = repeatEvery 24 light

par = 120.0

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



-----
idev = (*)
       <$> constant 0.374
       <*> (photo' <-*> constant 10.0)

idev' = (/)
       <$> idev
       <*> constant 4.0

idev'' = constant 0.626 <+*> idev'

pperiod =
  when (photo' <<*> constant 10.0) (constant 0.626) `orElse`
  (when (photo' <<*> constant 14.0) idev'' `orElse` constant 1.0)

thermal = when day (constant dayTemp') `orElse` constant 0.0
ptu = (*) <$> thermal <*> pperiod

tmin = -3.5
tmax = 6.0
wcsat = 960.0

favTemp temp = temp >= tmin && temp <= tmax

wcAcc wc t = wc + exp k * ((t-tmin)**o) * ((tmax-t)**ksi)
  where
    k   = -5.1748
    o   = 2.2256
    ksi = 0.99590

wcUpd t wc =
  if favTemp ctemp
    then wc'
    else wc
  where
    ctemp = at temp t
    wc' = min (wcAcc wc ctemp) wcsat

fp wc =
  if wc < wcsat
    then fp1
    else fp2
  where
    wcRat = wc / wcsat
    fp1 = 1 - fi + (fi - fu) * wcRat
    fp2 = 1 - fu

