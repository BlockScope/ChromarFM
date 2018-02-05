module RootExp where

import           Chromar.Fluent
import           Env
import           Plant

doExp tend tf = (rootDMptu, rootDThrm)
  where
    thrm = scanl (+) 0 [at temp t | t <- [1..tend]]
    wcAcc = scanl (\w t -> wcUpd t w) 0.0 [1..tend]

    mptu = scanl (+) 0 [at ptu t * fp w | (t, w) <- zip [1..tend] wcAcc]

    thrmF = thrm !! tf
    mptuF = mptu !! tf

    rootDMptu = [(t, rdem pt mptuF) | (t, pt) <- zip [1..tend] mptu]
    rootDThrm = [(t, rdem tt thrmF) | (t, tt) <- zip [1..tend] thrm ]
