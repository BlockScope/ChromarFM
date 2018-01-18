library(ncdf4)

d2m = "d2m"
ssr = "ssr"
t2m = "t2m"
tp = "tp"

valLat <- 10
valLon <- 10

svp <- function(t) {
    return(610.7*exp(17.4*t / (239+t)) / 1000)
}

mkFName <- function(env, year, month) {
    fn <- paste("../data/", env, "_",year,month, ".nc", sep="")

    return(fn)
}

mkMonthInst <- function(year, month, e, n) {
    tempd <- nc_open(mkFName(e, year, month))
    temps <- ncvar_get(tempd, e)

    npoints <- dim(temps)[3]

    dailyTemps <- rep(0, npoints / n )

    ind <- 1
    for (i in seq(1, npoints, n)) {
        dailyTemps[ind] <- mean(temps[valLat, valLon, i:(i+n-1)])
        ind <- ind + 1
    }
    
    return(dailyTemps)
}

mkMonthAcc <- function(year, month, e) {
    precd <- nc_open(mkFName(e, year, month))
    precps <- ncvar_get(precd, e)

    npoints <- dim(precps)[3]

    dailyPrecps <- rep(0, npoints/2)

    ind <- 1
    for (i in seq(1, npoints, 2)) {
        dailyPrecps[ind] <- precps[valLat, valLon, i] + precps[valLat, valLon, i+1]
        ind <- ind + 1
    }

    return(dailyPrecps)
}

mkMonthlyVpd <- function(temps, dtemps) {
    #assume length(temps) = length(dtemps)
    ndays <- length(temps)
    dvpds <- rep(0, ndays)
    
    for (i in 1:ndays) {
        dvpds[i] <- svp(temps[i]) - svp(dtemps[i])
    }

    return(dvpds)
}

mkMonth <- function(year, month) {
    dtemps <- mkMonthInst(year, month, t2m, 4) - 273.15
    ddtemps <- mkMonthInst(year, month, d2m, 4) - 273.15
    
    rads <- mkMonthAcc(year, month, ssr)
    dprecps <- mkMonthAcc(year, month, tp) * 1000
    
    dvpds <- mkMonthlyVpd(dtemps, ddtemps)

    envMonth <- data.frame(t2m=dtemps, rad=rads, rain=dprecps, vpd=dvpds)

    return(envMonth)
}
