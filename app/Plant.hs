{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Plant where

import           Chromar
import           Data.Fixed
import           Data.List
import           Data.Time.Calendar.MonthDay
import           Env
import           GHC.Exts
import           GHC.Generics
import           Params
import           Photo

log' t = 1.0 / (1.0 + exp (-100.0 * (t - 1000.0)))

logf' :: Double -> Double
logf' t = 1.0 / (1.0 + exp (-100.0 * (t - 2604.0)))

logs' :: Double -> Double
logs' t = 1.0 / (1.0 + exp (-100.0 * (t - 8448.0)))

tend = 1500

thrmFinal = sum [(at temp (fromIntegral ti)) / 24.0 | ti <- [1..tend]]

isLeaf :: Agent -> Bool
isLeaf Leaf {} = True
isLeaf _ = False

isPlant :: Agent -> Bool
isPlant (Plant{}) = True
isPlant _ = False

isEPlant :: Agent -> Bool
isEPlant (EPlant{}) = True
isEPlant _ = False

isSeed :: Agent -> Bool
isSeed (Seed{}) = True
isSeed _ = False

isSystem :: Agent -> Bool
isSystem (System{}) = True
isSystem _ = False

median :: [Int] -> Int
median [] = 0
median xs = (sort xs) !! mid
  where
    mid = length xs  `div` 2

avg l =
    let (t, n) = foldl' (\(b, c) a -> (a + b, c + 1)) (0, 0) l
    in (realToFrac t / realToFrac n)

getMonth time = m
  where
    hourYear = mod (floor time) (365*24) :: Int
    dayYear = hourYear `quot` 24
    (m, _) = dayOfYearToMonthAndDay False dayYear

plantD = Observable { name = "plantD",
                      gen = sumM dg . select isPlant }

eplantD = Observable { name = "plantD",
                      gen = sumM dg . select isEPlant }

plantTa = Observable { name = "plantTa",
                       gen = sumM ta . select isEPlant }

pCron' :: Double -> Double
pCron' dthr
    | dthr > 465 = adPCron
    | otherwise = juvPCron
  where
    juvPCron = 30.3
    adPCron = 11.9

lastTa :: Multiset Agent -> Double
lastTa mix = head (sortWith Down
                   [ta | (Leaf{i=i,m=_, a=_, ta=ta}, _) <- mix])

lastThr =
    Observable
    { name = "lastAppear"
    , gen = lastTa
    }

rateApp lta pc thrt =
    if thrt - lta > pc
        then 1.0
        else 0.0

nLeaves :: Multiset Agent -> Int
nLeaves mix =
    sum
        [ 1
        | (Leaf {}, _) <- mix ]

nL =
    Observable
    { name = "nLeaves"
    , gen = fromIntegral . nLeaves
    }

maxLeaf :: Multiset Agent -> Int
maxLeaf mix = max ((i . head) $ sortLeaves) 2
  where
    leaves = map fst $ select isLeaf mix
    sortLeaves = sortWith (Down . a) leaves

maxL =
    Observable
    { name = "maxL"
    , gen = fromIntegral . maxLeaf
    }

isRoot :: Agent -> Bool
isRoot Root {} = True
isRoot _ = False

rootMass =
    Observable
    { name = "rootMass"
    , gen = sumM m . select isRoot
    }

getAngle :: Int -> Int -> Int -> Double
getAngle i iMax nl
    | i <= iMax = minAngle
    | otherwise =
        minAngle +
        dAngle * (fromIntegral $ i - iMax) / (fromIntegral $ nl - iMax)

rosArea :: Multiset Agent -> Double
rosArea mix
    | nl <= 15 = sum lAreas
    | otherwise = sum $ take 13 (sortWith Down lAreas)
  where
    lAreas =
        [ a * ang
        | (Leaf {i = i
                ,m = m
                ,a = a}, _) <- mix
        , let ang = cos $ toRad (getAngle i iMax nl) ]
    nl = nLeaves mix
    iMax = maxLeaf mix
    toRad d = d / 180 * pi

rArea =
    Observable
    { name = "rosArea"
    , gen = rosArea
    }

raArea =
    Observable
    { name = "trueArea"
    , gen = sumM a . select isLeaf
    }

dassim phR ra = phR * ra * gc
  where
    gc = 86400 * 10**(-6)*12 * (1/24.0)

calcSDeg a b = 0.0

isSunset t = at day t && not (at day (t+1))

updSDeg s sdeg tt
  | isSunset tt = (s * kStarch) / (24 - (at photo' tt))
  | otherwise = sdeg

sla' thr = slaCot * exp (slaExp*thr)
  where
    slaCot = 0.144
    slaExp = -0.002

ldem i thra thr
  | thr - thra > texp i = 0.0
  | otherwise = dem / maxDem where
    a = 3.07
    b = 5.59
    dem = ((thr - thra + 0.5) / texp i)**(a-1) *
          (1 - ((thr - thra) / texp i))**(b-1)
    maxDem = 0.0161

rdem thrt = dem / maxDem
  where
    tr = rootLc * thrmFinal
    a = 13.03
    b = 9.58
    dem = ((thrt + 0.5) / tr) ** (a - 1) * (1 - (thrt + 0.5) / tr) ** (b - 1)
    maxPoint = tr / (1 + (b - 1) / (a - 1)) - 0.5
    maxDem =
        ((maxPoint + 0.5) / tr) ** (a - 1) *
        (1 - (maxPoint + 0.5) / tr) ** (b - 1)

texp :: Int -> Double
texp i
  | i < 3 = 300
  | otherwise = 400

m2c m = m * 0.3398

c2m c = c / 0.3398

rm2c m = m * rootFactor

rc2m c = c / rootFactor

maint :: Double -> Double -> Int -> Obs -> Obs -> Double -> Double
maint m a i iMax nl tempt = rlRes * leafArea * (1 / 24.0)
  where
    p = 0.085
    p' = 0.016
    c = m2c m
    toRad d = d / 180 * pi
    ang = toRad $ getAngle i (floor iMax) (floor nl)
    leafArea = a * (cos ang)
    rl20 = (p * c + p') * 24
    rlRes = rl20 * exp ((actE * (tempt - 20)) / (293 * 8.314 * (tempt + 273)))

maintRos m a tempt = rlRes * a * (1 / 24.0)
  where
    p = 0.085
    p' = 0.016
    c = m2c m
    rl20 = (p * c + p') * 24
    rlRes = rl20 * exp ((actE * (tempt - 20)) / (293 * 8.314 * (tempt + 273)))

-- growth in grams of mass for a leaf with mass m
g m = gmax * (1/24.0)
  where
    lc = m2c m
    gmax = 0.408 * lc

tLDem=
    Observable
    { name = "totGrowth"
    , gen =
        \s ->
             let tt =
                     sum
                         [ thr
                         | (EPlant {thrt = thr}, _) <- s ]
             in (sum
                    [ ldem i ta tt
                    | (Leaf {ta = ta
                            ,m = m
                            ,i = i}, _) <- s ])
    }

tRDem =
    Observable
    { name = "tRDem"
    , gen =
        \s ->
             let tt =
                     sum
                         [ thr
                         | (EPlant {thrt = thr}, _) <- s ]
             in rdem tt
    }


rsratio = Observable { name = "rsratio",
                       gen = \s -> (gen tRDem s) / (gen tLDem s) * 2.64 * 1.03}

grD =
    Observable
    { name = "grD"
    , gen =
        \s ->
             let rosMass = gen leafMass s
             in if (nLeaves s) > 0
                    then (1.2422 * g rosMass) +
                         (1.2422 * g rosMass * (gen rsratio s))
                    else 0.0
    }

cc s t =
    let cassim = cAssim s t
    in sum
           [ c
           | (Cell {c = c
                   ,s = s'}, _) <- s ] +
       cassim

grC s t =
    let rArea = rosArea s
        tMaint = totalMaint s t
    in (cc s t) - tMaint - (0.05 * rArea)

lMaint s t =
    let nl = nLeaves s
        iMax = maxLeaf s
    in if (nl > 0)
           then (sum
                     [ maint
                          m
                          a
                          i
                          (fromIntegral iMax)
                          (fromIntegral nl)
                          (at temp t)
                     | (Leaf {i = i
                             ,a = a
                             ,m = m}, _) <- s ])
           else 0.0

rMaint s t =
    let lmass = gen leafMass s
        larea = rosArea s
        rosMaint = maintRos lmass larea (at temp t)
    in sum
           [ rosMaint * (rm2c rm / m2c lmass)
           | (Root {m = rm}, _) <- s ]

totalMaint s t = rMaint s t + lMaint s t

cAssim s t =
    let phR = phRate (at temp t) (at par t) (at photo' t)
        rArea = rosArea s
    in 0.875 * (dassim phR rArea)

mkSt :: Multiset Agent
mkSt =
    ms
        (leaves ++ [ Cell{ c = initC * ra, s=initS * initC * ra}, root, plant ])
  where
    cotMass = cotArea / slaCot
    fR = rdem 100
    leaves =
        [ Leaf{ i = 1, ta = 0.0, m = cotArea/slaCot, a = cotArea}
        , Leaf{ i = 2, ta = 0.0, m = cotArea/slaCot, a = cotArea}
        ]
    root = Root { m = pr * fR * (seedInput / (pr*fR + 2)) }
    plant = Plant {thrt=0.0, attr=Attrs {ind = 1, psi = 0.0}, dg=0.0, wct=0.0}
    ra = rosArea (ms leaves)

mkSt' :: Multiset Agent
mkSt' = ms [ Plant {thrt=0.0, attr=Attrs {ind = 1, psi = 0.0}, dg=0.0, wct=0.0} ]

mkSt'' :: Multiset Agent
mkSt'' = ms [System{germTimes = [], flowerTimes=[], ssTimes=[], rosMass=[]},
             Seed {mass=1.6e-5, attr=Attrs {ind=1, psi=0.0}, dg=0.0, art=0.0}
            ]

leafMass = Observable { name = "mass",
                        gen = sumM m . select isLeaf }

leaf1Mass = Observable { name = "mass1",
                         gen = \s -> sum [m | (Leaf{i=i, m=m}, _) <- s, i == 1] }

leaf5Mass = Observable { name = "mass5",
                         gen = \s -> sum [m | (Leaf{i=i, m=m}, _) <- s, i == 5] }

leaf10Mass = Observable { name = "mass10",
                         gen = \s -> sum [m | (Leaf{i=i, m=m}, _) <- s, i == 10] }

leaf12Mass = Observable { name = "mass12",
                         gen = \s -> sum [m | (Leaf{i=i, m=m}, _) <- s, i == 12] }

data Attrs = Attrs
  { ind :: Int
  , psi :: Double
  } deriving (Ord, Eq, Show)

data Agent
    = System { germTimes   :: [Int]
             , flowerTimes :: [Int]
             , ssTimes     :: [Int]
             , rosMass     :: [Double]}
    | Seed { mass :: Double
           , attr :: Attrs
           , dg   :: Double
           , art  :: Double}
    | Leaf { i  :: Int
           , ta :: Double
           , m  :: Double
           , a  :: Double}
    | Cell { c :: Double
           , s :: Double}
    | Root { m :: Double}
    | Plant { thrt :: Double
            , attr :: Attrs
            , dg   :: Double
            , wct  :: Double}
    | EPlant { sdeg :: Double
             , thrt :: Double
             , attr :: Attrs
             , dg   :: Double
             , wct  :: Double}
    | FPlant { attr :: Attrs
             , dg   :: Double}
    deriving (Eq, Ord, Show)

isCell (Cell{c=c}) = True
isCell _ = False

--s2c :: Double -> Double
s2c s pp = (kStarch * s) / (24 - pp)

emerg d
  | d > 110 = 1.0
  | otherwise = 0.0

$(return [])
----- rules -------

dev =
    [rule| Seed{attr=atr, dg=d, art=a} -->
           Seed{attr=atr, dg = d + (htu time a (psi atr)), art=a + (arUpd moist temp)}
           @1.0
   |]

trans =
    [rule|
        System{germTimes=gts}, Seed{mass=m, attr=atr, dg=d, art=a} -->
        System{germTimes=(getMonth time:gts)}, Plant{thrt=0.0, attr=atr, dg=0.0, wct=0.0}
        @log' d
  |]

growth =
    [rule|
      EPlant{thrt=tt}, Leaf{i=i, m=m, a=a, ta=ta}, Cell{c=c, s=s'} -->
      EPlant{thrt=tt}, Leaf{m=m+(c2m gr), a=max a a'}, Cell{c=c-grRes, s=s'}
      @10*ld [c-grRes > cEqui]
        where
          ld = ldem i ta tt,
          cEqui = 0.05 * rArea,
          gr = (g leafMass) / 10,
          a' = (sla' tt) * (m + (c2m gr)),
          grRes = 1.2422 * gr
    |]

assim =
  [rule|
    Cell{c=c, s=s'} -->
    Cell{c=c + 0.875*da, s=s'+ 0.125*da}
    @1.0 [day]
      where
        da = dassim (phRate temp par photo')  rArea
  |]

starchConv =
  [rule|
    EPlant{sdeg=sd}, Cell{c=c, s=s'} -->
    EPlant{sdeg=sd}, Cell{c=c+sd, s=s'-sd} @1.0 [not day && (s'-sd > 0.0)]
  |]

starchFlow =
    [rule| Cell{c=c, s=s'} --> Cell{c=c-extra, s=s'+extra} @10.0 [c - extra > 0.0 && day]
          where
            extra = (max 0.0 (grC s t - grD)) / 10.0 |]

leafCr =
    [rule|
      EPlant{thrt=tt} --> EPlant{thrt=tt}, Leaf{i=(floor nL+1), ta=tt, m=cotArea/slaCot, a=cotArea}
      @(rateApp lastThr (pCron' tt) tt)
    |]

maintRes =
  [rule|
    Cell{c=c, s=s'}, Leaf{a=a,i=i, m=m} -->
    Cell{c=c-lmaint}, Leaf{m=m}
    @1.0 [c-lmaint > 0]
      where
        lmaint = maint m a i maxL nL temp |]

maintRes' =
  [rule|
   Cell{c=c, s=s'} -->
   Cell{c=c-lmaint} @1.0 [c-lmaint > 0]
     where
       lmaint = maintRos leafMass rArea temp |]

rootGrowth =
  [rule|
    EPlant{thrt=tt}, Root{m=m}, Cell{c=c, s=s'} -->
    EPlant{thrt=tt}, Root{m=m+ rc2m rg}, Cell{c=c-rgRes, s=s'}
    @10*(rdem tt) [c - rgRes > cEqui]
      where
        cEqui = 0.05 * rArea,
        rg = (pr * g leafMass) / 10.0,
        rgRes = 1.2422 * rg
  |]

rootMaint =
  [rule|
    Root{m=m}, Cell{c=c, s=s'} -->
    Root{m=m}, Cell{c=c-rm, s=s'}
    @1.0 [c-rm > 0.0]
      where
        rm = (rm2c m)/(m2c leafMass) * (maintRos leafMass rArea temp)
  |]

leafTransl =
  [rule|
    Leaf{m=lm}, Root{m=rm}, Cell{c=c, s=s'} -->
    Leaf{m=lm-c2m tl}, Root{m=rm}, Cell{c=c+tl}
    @1.0 [c <= cEqui]
      where
        cEqui = 0.05 * rArea,
        tl = (m2c lm / (m2c leafMass + rm2c rm)) * (cEqui - c)
  |]

rootTransl =
  [rule|
     Root{m=rm}, Cell{c=c, s=s'} -->
     Root{m=rm-rc2m tl}, Cell{c=c+tl, s=s'}
     @1.0 [c <= cEqui]
       where
         cEqui = 0.05 * rArea,
         tl = (rm2c rm / (m2c leafMass + rm2c rm)) * (cEqui - c)
  |]

devp =
  [rule| Plant{thrt=tt, dg=d, wct=w} -->
         Plant{thrt=tt+(temp / 24.0),
               dg=d+ptu* fp (wcUpd time w),
               wct=wcUpd time w}
         @1.0 |]

devep =
    [rule| Cell{s=s'}, EPlant{sdeg=sd, thrt=tt, dg=d, wct=w} -->
           Cell{s=s'}, EPlant{sdeg=updSDeg s' sd t, thrt=tt+(temp / 24.0),
                              dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w}
           @1.0 |]

eme =
  [rule| Plant{thrt=tt, attr=ar, dg=d, wct=w} -->
         EPlant{sdeg=calcSDeg si time, thrt=tt, attr=ar, dg=d, wct=w},
          Leaf{ i = 1, ta = tt, m = cotArea/slaCot, a = cotArea},
          Leaf{ i = 2, ta = tt, m = cotArea/slaCot, a = cotArea},
          Root { m = pr * fR * (seedInput / (pr*fR + 2)) },
          Cell{ c = initC * ra, s=si} @emerg tt [True]
            where
              cotMass = cotArea / slaCot,
              fR = rdem tt,
              ra = 2*cotArea*cos (10/180*pi),
              si = initS * initC * ra
  |]

leafD =
  [rule| FPlant{dg=d}, Leaf{ta=ta} --> FPlant{dg=d} @1.0 |]

leafD' =
  [rule| EPlant{thrt=tt}, Leaf{ta=ta} --> EPlant{thrt=tt} @1.0 [tt > ts + ta] |]

transp =
    [rule|
        System{flowerTimes=fts, rosMass=rms},
        EPlant{attr=atr, dg=d, wct=w}, Root{m=m}, Cell{c=c, s=s'} -->
        System{flowerTimes=(getMonth time:fts), rosMass=(leafMass:rms)},
        FPlant{attr=atr, dg=0.0}
        @logf' d
    |]

devfp =
    [rule| FPlant{dg=d} --> FPlant{dg=d+disp} @1.0 |]

transfp =
    [rule|
         System{ssTimes=ss}, FPlant{attr=atr, dg=d} -->
         System{ssTimes=(getMonth time:ss)},
         Seed{mass=1.6e-5, attr=atr, dg=0.0, art=0.0}
         @logs' d
   |]

rootD = undefined

md =
    Model
    { rules =
        [ dev
        , trans
        , growth
        , assim
        , leafCr
        , starchConv
        , maintRes
        , rootGrowth
        , rootMaint
        , leafTransl
        , rootTransl
        , devp
        , devep
        , eme
        , leafD'
        , leafD
        , transp
        , devfp
        , transfp
        ]
    , initState = mkSt''
    }

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

carbon =
    Observable
    { name = "carbon"
    , gen = sumM c . select isCell
    }

starch =
    Observable
    { name = "starch"
    , gen = sumM s . select isCell
    }

m1 =
    Observable
    { name = "mass1"
    , gen = sumM m . selectAttr i 1 . select isLeaf
    }

m2 =
    Observable
    { name = "mass2"
    , gen = sumM m . selectAttr i 2 . select isLeaf
    }

seedD = Observable { name = "seedD",
                     gen = sumM dg . select isSeed }

thrtt = Observable { name = "thrtt",
                     gen = sumM thrt . select isEPlant }

avgGermMonth = Observable { name = "avgGermM",
                            gen = avgGerm }
  where
    avgGerm mix = fromIntegral (median (head [gts | (System{germTimes=gts}, _) <- mix]))

avgFlMonth = Observable { name = "avgFlM",
                            gen = avgFl }
  where
    avgFl mix = fromIntegral (median (head [fts | (System{flowerTimes=fts}, _) <- mix]))

avgSMonth = Observable { name = "avgSM",
                         gen = avgS }
  where
    avgS mix = fromIntegral (median (head [ss | (System{ssTimes=ss}, _) <- mix]))

avgRosMass = Observable { name = "avgRosMass",
                          gen = avgMass }
  where
    avgMass mix = avg (head [rms | (System{rosMass=rms}, _) <- mix ])

trdem =
    Observable
    { name = "rdem"
    , gen =
        \s ->
             let tt =
                     sum
                         [ thr
                         | (EPlant {thrt = thr}, _) <- s ]
             in rdem tt
    }

sdg = Observable { name="sdeg", gen= \s -> sum [sd | (EPlant{sdeg=sd}, _) <- s]}

hasFlowered :: Multiset Agent -> Bool
hasFlowered mix = (sumM dg . select isEPlant) mix < 3212


