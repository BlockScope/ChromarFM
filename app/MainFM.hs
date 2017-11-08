report e fout gts fts ss rms = appendFile fout (unlines rows)
  where
    tag = "f" ++ show (frepr e) ++ "_" ++ "d" ++ show (psim e)
    out (gt, ft, ss, rm, tag) =
        show gt ++
        " " ++ show ft ++ " " ++ show ss ++ " " ++ show rm ++ " " ++ tag
    header =
        "germT" ++
        " " ++
        "flowerT" ++ " " ++ "ssetT" ++ " " ++ "rosMass" ++ " " ++ "params"
    rows =
        header :
        (map
             out
             (zip5
                  (reverse gts)
                  (reverse fts)
                  (reverse ss)
                  (reverse rms)
                  (repeat tag)))

mainDistr :: FilePath -> (Double, Double) -> IO ()
mainDistr fout (pm, fr) = do
    print (pm, fr)
    let e = Env { psim = pm, frepr = fr}
        mdE = md e
    gen <- R.getStdGen
    let tend = (365 * 50 * 24)
    let traj =
            takeWhile
                (\s -> getT s < tend)
                (simulate gen (rules mdE) (initState mdE))
    let lState = getM (last traj)
    let rms =
            head
                [ rm
                | (System {rosMass = rm}, _) <- lState ]
    let (gts, fts, ss) =
            head
                [ (gt, ft, s)
                | (System {germTimes = gt
                          ,flowerTimes = ft
                          ,ssTimes = s}, _) <- lState ]
    let gts =
            head
                [ gt
                | (System {germTimes = gt}, _) <- lState ]
    report e fout gts fts ss rms

mainLife :: IO ()
mainLife = do
  let pms = [0.0, 2.5]
      frs = [0.598, 0.737]
  forM_ [(p, f) | p <- pms, f <- frs] (mainDistr "out/lifeExpsVal/lifecycles.txt")
