module Env where

import Chromar.Fluent

psmax = -5
psmin = -1
psu = -50
psl = -350
psSc = 1.0
tbar = 3.0
tbg = 3.0
tbd = 3.0
kt = 0.12
to = 22

fi = 0.5741
fu = 0


dayTemp = 23.0 :: Double
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

moist = constant (1.1)



--------- plant dev -----
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


--------seed dev --------------

arUpd moist temp
  | moist <= psmax && moist >= psu = temp - tbar
  | moist < psu && moist > psl = ((psl - moist) / (psl - psu)) * (temp - tbar)
  | moist <= psmax || moist <= psl = 0.0
  | otherwise = 0.0

psB ar psi =
  if psb' > psmin
     then psb'
     else psmin
  where
    arlab = arUpd (-200) 20
    dsat = 40
    psb' = psi - psSc * (ar / (arlab * dsat * 24) )

htuSub ar psi moist temp = (moist - psB ar psi) * (temp - tbg)

htuOpt ar psi moist temp = (moist - mpsB) * (to - tbg)
  where
    mpsB = psB ar psi + kt * (temp - to)

---t : time
--- a : afterripening
--- psi : initial dorm
htu t a psi
  | moistt > psb && tempt > tbg && tempt < to = htuSub ar psi moistt tempt
  | mpsB < moistt && tempt > to = htuOpt a psi moistt tempt
  | otherwise = 0.0                                
  where
    tempt = at temp t
    moistt = at moist t
    ar = a + arUpd moistt tempt
    psb = psB ar psi
    mpsB = psB ar psi + kt * (tempt-to)


---- infloresence dev -----
disp = when (ntemp <>*> constant 0.0) ntemp `orElse` (constant 0.0)
  where
    ntemp = temp <-*> constant tbd

