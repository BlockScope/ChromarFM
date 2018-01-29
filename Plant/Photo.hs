module Photo where

import           Env

r = 8.314 :: Double

aEnergyKc25 = 59400.0 :: Double

kmRsco25 = 40.4 :: Double

aEnergyKo25 = 36000.0 :: Double

kmRubO = 24800 :: Double

aEnergyVlmax = 64800 :: Double

o2ParPre = 20500 :: Double

aEnergyJm25 = 37000 :: Double

hCurv = 220000 :: Double

sElec = 710 :: Double

fspec = 0.15 :: Double

thCurv = 0.7 :: Double

vlMax20 = 19.0 :: Double

vlMax25 = vlMax20 / 0.64

jv d
    | (d == 4) || (d == 6) || (d == 8) = 2.1
    | d == 12 = 1.7
    | d == 14 = 1.4
    | d == 18 = 1.2
    | otherwise = 1.0

fstom m = sqrt fw
  where
    moistInd = (m + 200) / 200
    fw = min (max 0.01 ((moistInd - 0.6)/0.2)) 1

fstom' :: Double -> Double
fstom' m = sqrt (min (max 0.01 ((moistInd - 0.6)/0.2)) 1)
  where
    mmax = -10.0
    moistInd = (mmax / m) ** (1/5.0)

phRate tempt tpar pp m
  | tempt <= 0.0 = 0.0
  | otherwise = if rho <= 0
                then avRub
                else min avRub ajRub                 
  where
    dn = 298 * r * (tempt + 273.0)
    kcTLeaf = kmRsco25 * exp (aEnergyKc25 * (tempt - 25) / dn)
    koTLeaf = kmRubO * exp (aEnergyKo25 * (tempt - 25) / dn)
    vlMaxTLeaf = vlMax25 * exp (aEnergyVlmax * (tempt - 25) / dn)
    k' = kcTLeaf * (1 + o2ParPre / koTLeaf)
    co2CompPoint = 3.68 + 0.188 * (tempt - 25) + 0.0036 * (tempt - 25) ** 2
    intCo2 = 0.7 * co2
    avRub = vlMaxTLeaf * (intCo2 - co2CompPoint) / (intCo2 + k')
    t1 = exp ((aEnergyJm25 * (tempt - 25)) / (298 * r * (tempt + 273)))
    t2 = 1 + exp ((sElec * 298 - hCurv) / (r * 298))
    t3 = 1 + exp ((sElec * (273 + tempt) - hCurv) / (r * (273.0 + tempt)))
    jmPot = (jv pp) * vlMax25 * t1 * t2 / t3
    ilePar = tpar * (1 - fspec) / 2.0
    bb = -1 * (ilePar + jmPot)
    cc = ilePar * jmPot
    rho = bb ** 2 - 4 * thCurv * cc
    sol =
        if rho <= 0
            then 0
            else (-1 * bb - rho ** 0.5) / (2 * thCurv)
    ajRub = (sol * (intCo2 - co2CompPoint)) / (4 * (intCo2 + 2 * co2CompPoint))

phRate' = 6.2564

phRate12 = 5.614

phRate14 = 4.9855

phRate18 = 4.491
