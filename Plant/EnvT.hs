module EnvT where

import           Chromar.Fluent
import           Data.Fixed
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T
import qualified Data.Text.IO     as TI
import           Params
import           System.IO.Unsafe

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

fu = 0

dataFile = "data/soil/valencia_20102011.csv"
--dataFile = "data/weatherValencia60yrs.csv"

shift :: Time -> Fluent a -> Fluent a
shift t0 f = mkFluent (\t -> at f (t + t0))

-- temp' = shift 6864 (repeatEvery 17520 (unsafePerformIO (readTable dataFile 3)))
-- photo' = shift 6864 (repeatEvery 17520 (unsafePerformIO (readTable dataFile 1)))
-- day' = shift 6864 (repeatEvery 17520 (unsafePerformIO (readTable dataFile 2)))
-- moist = shift 6864 (repeatEvery 17520 (unsafePerformIO (readTable dataFile 4)))
-- par = shift 6864 (repeatEvery 17520 (unsafePerformIO (readTable dataFile 5)))
-- day  = day' <>*> constant 0.0

sunrise = 6
sunset = 18 :: Double
temp' = constant 22.0
photo' = constant (sunset - sunrise)
light = between sunrise sunset (constant True) (constant False)
day = repeatEvery 24 light
moist = constant 1.0
par = constant 120.0

tempBase = constant 3.0
temp = max <$> (temp' <-*> tempBase) <*> pure 0.0

co2 = 42.0

--------- plant dev -----
idev = (*)
       <$> constant 0.3985
       <*> (photo' <-*> constant 10.0)

idev' = (/)
       <$> idev
       <*> constant 4.0

idev'' = constant 0.6015 <+*> idev'

pperiod =
  when (photo' <<*> constant 10.0) (constant 0.6015) `orElse`
  (when (photo' <<*> constant 14.0) idev'' `orElse` constant 1.0)

-- lightFr t
--   | td <= sunrise || td >= sunset + 1 = 0
--   | td >= sunrise + 1 && td < sunset = 1
--   | td <= sunrise + 1 = td - sunrise
--   | otherwise = sunset - td + 1
--   where
--     td = mod' t 24

-- thrm t
--     | lightFr t == 0 = pN * (max tempt 0)
--     | lightFr t == 1 = (max tempt 0)
--     | otherwise =
--         max 0 (tempt * lightFr t) + pN * (max 0 tempt * (1 - lightFr t))
--   where
--     tempt = at temp t

---thermal = mkFluent thrm

thermal = when day temp `orElse` constant 0.0

ptu = (*) <$> thermal <*> pperiod

tmin = -3.5
tmax = 6.0
wcsat = 960.0

favTemp temp = temp >= tmin && temp <= tmax

wcAcc wc t = wc + exp k * ((t-tmin)**o) * ((tmax-t)**ksi)
  where
    k   = -5.17
    o   = 2.23
    ksi = 1

wcUpd t wc =
  if favTemp ctemp
    then wc'
    else wc
  where
    ctemp = at temp t
    wc' = min (wcAcc wc ctemp) wcsat

fp wc fi =
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
disp = when (ntemp <>*> constant 0.0) ntemp `orElse` constant 0.0
  where
    ntemp = temp <-*> constant tbd

--------------
parseLine :: Int -> T.Text -> (Double, Double)
parseLine n ln = (read $ T.unpack time, read $ T.unpack temp) where
  elems = T.splitOn (T.pack ",") ln
  time = elems !! 0
  temp = elems !! n


readTable :: FilePath -> Int -> IO (Fluent Double)
readTable fn n = do
  contents <- TI.readFile fn
  let vals = map (parseLine n) (T.lines contents)
  return $ flookupM (Map.fromList vals)

------

