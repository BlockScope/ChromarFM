library(ncdf4)

d2m = "d2m"
ssr = "ssr"
t2m = "t2m"
tp = "tp"
eva = "e"
mn2t = "mn2t"
swvl1 = "swvl1"
swvl2 = "swvl2"
swvl3 = "swvl3"

#location indexes based on 1.5x1.5 grid over Europe
val = c(23, 18)
oul = c(7, 36)
edin = c(13, 17)
hal = c(16, 27)
nor = c()

dayLengths <- function(loc, dates) {
    srise <- sunriset(loc, dates, direction="sunrise", POSIXct.out=TRUE)
    sset <- sunriset(loc, dates, direction="sunset", POSIXct.out=TRUE)

    dlens <- as.numeric(sset$time - srise$time)
    
    return(dlens / 24.0)
}

mkDLengths <- function(fn, loc, dates) {
    d <- nc_open(fn)
    lons <- ncvar_get(d, "longitude")
    lats <- ncvar_get(d, "latitude")

    return(dayLengths(matrix(c(lons[loc[2]], lats[loc[1]]), nrow=1), dates))
}

mkFName <- function(env, year, month) {
    fn <- paste("../data/", env, "_",year,month, ".nc", sep="")

    return(fn)
}

aggrDaily <- function(fn, e, loc, n, f) {
    lat <- loc[1]
    lon <- loc[2]
    d <- nc_open(fn)
    vals <- ncvar_get(d, e)

    npoints <- dim(vals)[3]

    dailyVals <- rep(0, npoints / n )

    ind <- 1
    for (i in seq(1, npoints, n)) {
        dailyVals[ind] <- f(vals[lon, lat, i:(i+n-1)])
        ind <- ind + 1
    }

    nc_close(d)
    return(dailyVals)
}

processWeather <- function(year, month, loc, dates) {
    tempFile <- mkFName(t2m, year, month)
    dtempFile <- mkFName(d2m, year, month)
    radFile <- mkFName(ssr, year, month)
    precFile <- mkFName(tp, year, month)

    #use min temps instead of temps
    temps <- aggrDaily(tempFile, t2m, loc, 4, min) - 273.15
    rads <- aggrDaily(radFile, ssr, loc, 2, sum) / 1000000
    dtemps <- aggrDaily(dtempFile, d2m, loc, 2,  mean) - 273.15
    precs <- aggrDaily(precFile, tp, loc, 2, sum) * 1000

    env <- data.frame(precs=precs,
                      temps=temps,
                      rads=rads,
                      dtemps=dtemps,
                      ds=mkDLengths(tempFile, loc, dates))

    return(env)
}

indexifySW <- function(swp, sfc) {
    return(function(sw) {
        return((sw - swp) / (sfc - swp))
    })
}

zipWith <- function(f, xs, ys) {
    n <- length(xs)
    zs <- rep(0, n)
    
    for (i in 1:n) {
        zs[i] <- f(c(xs[i], ys[i]))
    }

    return(zs)
}

zipWith3 <- function(f, xs, ys, zs) {
    n <- length(xs)
    res <- rep(0, n)

    for (i in 1:n) {
        res[i] <-  f(c(xs[i], ys[i], zs[i]))
    }

    return(res)
}

mkSWIndex <- function(year, month, loc) {
    sw1 <- aggrDaily(mkFName(swvl1, year, month), swvl1, loc, 4, mean)
    sw2 <- aggrDaily(mkFName(swvl2, year, month), swvl2, loc, 4, mean)

    return(sapply(zipWith(mean, sw1, sw2),
                  indexifySW(0.151, 0.346)))
}

mkSWIndex3 <- function(year, month, loc) {
    sw1 <- aggrDaily(mkFName(swvl1, year, month), swvl1, loc, 4, mean)
    sw2 <- aggrDaily(mkFName(swvl2, year, month), swvl2, loc, 4, mean)
    sw3 <- aggrDaily(mkFName(swvl3, year, month), swvl3, loc, 4, mean)

    return(sapply(zipWith3(mean, sw1, sw2, sw3),
                  indexifySW(0.151, 0.346)))
}


mkSWIndex1 <- function(year, month, loc) {
    sw1 <- aggrDaily(mkFName(swvl1, year, month), swvl1, loc, 4, mean)

    return(sapply(sw1,
                  indexifySW(0.151, 0.346)))
}

mkSWIndex1Soil <- function(year, month, loc, spwp, sfc) {
    sw1 <- aggrDaily(mkFName(swvl1, year, month), swvl1, loc, 4, mean)

    return(sapply(sw1,
                  indexifySW(spwp, sfc)))
}
