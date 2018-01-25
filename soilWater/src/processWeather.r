library(ncdf4)
library(oce)

d2m = "d2m"
ssr = "ssr"
t2m = "t2m"
tp = "tp"
eva = "e"
mn2t = "mn2t"

#location indexes based on 1.5x1.5 grid over Europe
val = c(24, 19)
oul = c(7, 2)
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
    
    return(dailyVals)
}

processWeather <- function(year, month, loc, dates) {
    tempFile <- mkFName(t2m, year, month)
    dtempFile <- mkFName(d2m, year, month)
    radFile <- mkFName(ssr, year, month)
    precFile <- mkFName(tp, year, month)

   #use min temps instead of temps for all these calculations
    temps <- aggrDaily(tempFile, t2m, loc, 4, min) - 273.15
    rads <- aggrDaily(radFile, ssr, loc, 2, sum) / 1000000
    dtemps <- aggrDaily(dtempFile, d2m, loc, 4,  mean) - 273.15
    precs <- aggrDaily(precFile, tp, loc, 2, sum) * 1000

    env <- data.frame(precs=precs,
                      temps=temps,
                      rads=rads,
                      dtemps=dtemps,
                      ds=mkDLengths(tempFile, loc, dates))

    return(env)
}
