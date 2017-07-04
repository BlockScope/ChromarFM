{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE  TemplateHaskell #-}

module Plant where

import GHC.Exts
import GHC.Generics
import Chromar
import Env
import Params
import Photo

log' t = 1.0 / (1.0 + exp (-100.0 * (t - 1000.0)))

logf' :: Double -> Double
logf' t = 1.0 / (1.0 + exp (-100.0 * (t - 2604.0)))

logs' :: Double -> Double
logs' t = 1.0 / (1.0 + exp (-100.0 * (t - 8448.0)))

tend = 800

thrmFinal = at thr tend

isLeaf :: Agent -> Bool
isLeaf (Leaf {}) = True
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

plantD = Observable { name = "plantD",
                      gen = sumM dg . select isPlant }

eplantD = Observable { name = "plantD",
                      gen = sumM dg . select isEPlant }

plantTa = Observable { name = "plantTa",
                       gen = sumM ta . select isEPlant }

pCron :: Fluent Double
pCron = when (thr <>*> (constant 465)) adPCron `orElse` juvPCron
  where
    juvPCron = constant 30.3
    adPCron = constant 11.9

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
        then 1.0 -- / 24.0
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

sla =
    (*) <$> constant slaCot <*>
    (exp <$> ((*) <$> constant slaExp <*> (thr <-*> constant 100)))
  where
    slaCot = 0.144
    slaExp = -0.002

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

-- growth in grams of mass for a leaf with mass m
g m = gmax * (1/24.0)
  where
    lc = m2c m
    gmax = 0.408 * lc

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
    plant = Plant {attr=Attrs {ind = 1, psi = 0.0}, dg=0.0, wct=0.0}
    ra = rosArea (ms leaves)

mkSt' :: Multiset Agent
mkSt' = ms [ Plant {attr=Attrs {ind = 1, psi = 0.0}, dg=0.0, wct=0.0} ]

mkSt'' :: Multiset Agent
mkSt'' = ms [Seed {mass=1.6e-5, attr=Attrs {ind=1, psi=0.0}, dg=0.0, art=0.0} ]

leafMass = Observable { name = "mass",
                        gen = sumM m . select isLeaf }

data Attrs = Attrs
  { ind :: !Int
  , psi :: !Double
  } deriving (Ord, Eq, Show)
             
data Agent
    = Seed { mass :: !Double
           , attr :: !Attrs
           , dg :: !Double
           , art :: !Double}
    | Leaf { i :: !Int
           , ta :: !Double
           , m :: !Double
           , a :: !Double }
    | Cell { c :: !Double, s :: !Double }
    | Root { m :: !Double }
    | Plant { attr :: !Attrs
            , dg :: !Double
            , wct :: !Double}
    | EPlant { ta :: !Double
             , attr :: !Attrs
             , dg :: !Double
             , wct :: !Double}
    | FPlant { attr :: !Attrs
             , dg :: !Double}  
    deriving (Eq, Ord, Show)

isCell (Cell{c=c}) = True
isCell _ = False

s2c :: Double -> Double
s2c s = (kStarch * s) / (24 - d)

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
        Seed{mass=m, attr=atr, dg=d, art=a} -->
        Plant{attr=atr, dg=0.0, wct=0.0}
        @log' d
  |]

growth =
    [rule|
      Leaf{i=i, m=m, a=a, ta=ta}, Cell{c=c, s=s'} -->
      Leaf{m=m+(c2m $ gr), a=max a a'}, Cell{c=c-grRes, s=s'}
      @10*ld [c-grRes > cEqui]
        where
          gr = (g leafMass) / 10.0,
          a' = (sla' (thr-plantTa)) * (m + gr),
          ld = ldem i ta thr,
          grRes = 1.24 * gr,
          cEqui = 0.05 * rArea
    |]

assim =
  [rule|
    Cell{c=c, s=s'} -->
    Cell{c=c + 0.875*da, s=s'+ 0.125*da}
    @1.0 [day]
      where
        da = dassim (phRate temp) rArea
  |]

starchConv =
  [rule|
    Cell{c=c, s=s'} -->
    Cell{c=c+(s2c s'), s=s'-(s2c s')} @1.0 [not day]
  |]

leafCr =
    [rule|
      EPlant{ta=ta} --> EPlant{ta=ta}, Leaf{i=(floor nL+1), ta=thr, m=cotArea/slaCot, a=cotArea}
      @(rateApp lastThr (pCron' (thr-ta)) thr)
    |]

maintRes =
  [rule|
    Cell{c=c, s=s'}, Leaf{a=a,i=i, m=m} -->
    Cell{c=c-lmaint}, Leaf{m=m}
    @1.0 [c-lmaint > 0]
      where
        lmaint = maint m a i maxL nL temp |]

rootGrowth =
  [rule|
    Root{m=m}, Cell{c=c, s=s'} -->
    Root{m=m+ rc2m rg}, Cell{c=c-rg, s=s'}
    @rdem thr [c-rg > cEqui]
      where
        rg = pr*g leafMass,
        cEqui = 0.05 * rArea
  |]

rootMaint =
  [rule|
    Root{m=m}, Cell{c=c, s=s'} -->
    Root{m=m}, Cell{c=c-rm, s=s'}
    @1.0 [c-rm > 0.0]
      where
        rm = maint m rArea (floor maxL) maxL nL temp
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
  [rule| Plant{dg=d, wct=w} -->
         Plant{dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w} @1.0 |]

devep =
  [rule| EPlant{dg=d, wct=w} -->
         EPlant{dg=d+ptu* fp (wcUpd time w), wct=wcUpd time w} @1.0 |]

eme =
  [rule| Plant{attr=ar, dg=d, wct=w} -->
         EPlant{ta=thr, attr=ar, dg=d, wct=w},
          Leaf{ i = 1, ta = thr, m = cotArea/slaCot, a = cotArea},
          Leaf{ i = 2, ta = thr, m = cotArea/slaCot, a = cotArea},
          Root { m = pr * fR * (seedInput / (pr*fR + 2)) },
          Cell{ c = initC * ra, s=initS * initC * ra} @emerg d [True]
            where
              cotMass = cotArea / slaCot,
              fR = rdem 100,
              ra = 2*cotArea*cos (10/180*pi)
  |]
  
leafD =
  [rule| FPlant{dg=d}, Leaf{ta=ta} --> FPlant{dg=d} @1.0 |]

transp =
    [rule|
        EPlant{attr=atr, dg=d, wct=w}, Root{m=m}, Cell{c=c, s=s'} -->
        FPlant{attr=atr, dg=0.0}
        @logf' d
    |]

devfp =
    [rule| FPlant{dg=d} --> FPlant{dg=d+disp} @1.0 |]

transfp =
    [rule|
         FPlant{attr=atr, dg=d} -->
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
        , leafD
        , transp
        , devfp
        , transfp 
        ]
    , initState = mkSt''
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


hasFlowered :: Multiset Agent -> Bool
hasFlowered mix = (sumM dg . select isEPlant) mix < 2604
