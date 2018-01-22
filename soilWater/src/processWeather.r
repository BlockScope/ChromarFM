library(ncdf4)

d2m = "d2m"
ssr = "ssr"
t2m = "t2m"
tp = "tp"
eva = "e"

val = c(24, 19)
oul = c(7, 2)
edin = c(13, 17)
hal = c(16, 27)
nor = c()

lw <- function() {
    x <- c(-5, 0, 5, 10, 15, 20, 25, 30, 35, 40)
    y <- c(2.51, 2.5, 2.49, 2.48, 2.47, 2.45, 2.44, 2.43, 2.42, 2.41)
    
    return(approxfun(x, y))
}

gm <- function() {
    x <- c(-5, 0, 5, 10, 15, 20, 25, 30, 35, 40)
    y <- c(0.527, 0.521, 0.515, 0.509, 0.503, 0.495, 0.488, 0.482, 0.478, 0.474)

    return(approxfun(x, y))
}

s <- function() {
    x <- c(-5, 0, 5, 10, 15, 20, 25, 30, 35, 40)
    y <- c(0.24, 0.33, 0.45, 0.6, 0.78, 1.01, 1.3, 1.65, 2.07, 2.57)

    return(approxfun(x, y))
}

revap <- function(temps, rads) {
    ndays <- length(temps)

    revap <- rep(0, ndays)

    for (i in 1:ndays) {
        lwt <- lw()(temps[i])
        gt <- gm()(temps[i])
        st <- s()(temps[i])
        
        revap[i] <- rads[i] / (lwt * (1 + (gt / st)))
    }

    return(revap)
}

svp <- function(t) {
    return(610.7*exp(17.4*t / (239+t)) / 1000)
}

mkFName <- function(env, year, month) {
    fn <- paste("../data/", env, "_",year,month, ".nc", sep="")

    return(fn)
}

mkMonthInst <- function(year, month, e, n, loc) {
    lat <- loc[1]
    lon <- loc[2]
    tempd <- nc_open(mkFName(e, year, month))
    temps <- ncvar_get(tempd, e)

    npoints <- dim(temps)[3]

    dailyTemps <- rep(0, npoints / n )

    ind <- 1
    for (i in seq(1, npoints, n)) {
        dailyTemps[ind] <- mean(temps[lon, lat, i:(i+n-1)])
        ind <- ind + 1
    }
    
    return(dailyTemps)
}

mkMonthAcc <- function(year, month, e, loc) {
    lat = loc[1]
    lon = loc[2]
    precd <- nc_open(mkFName(e, year, month))
    precps <- ncvar_get(precd, e)

    npoints <- dim(precps)[3]

    dailyPrecps <- rep(0, npoints/2)

    ind <- 1
    for (i in seq(1, npoints, 2)) {
        dailyPrecps[ind] <- precps[lon, lat, i] + precps[lon, lat, i+1]
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

mkMonth <- function(year, month, loc) {
    dtemps <- mkMonthInst(year, month, t2m, 4, loc) - 273.15
    ddtemps <- mkMonthInst(year, month, d2m, 4, loc) - 273.15
    
    rads <- mkMonthAcc(year, month, ssr, loc)
    dprecps <- mkMonthAcc(year, month, tp, loc) * 1000
    
    dvpds <- mkMonthlyVpd(dtemps, ddtemps)
    evaps <- mkMonthAcc(year, month, eva, loc) * 1000

    envMonth <- data.frame(t2m=dtemps, rad=rads, rain=dprecps, vpd=dvpds, e=evaps)

    return(envMonth)
}

mkYear <- function(year, loc) {
    dprecps <- mkMonthAcc(year, "", tp, loc) * 1000
    evaps <- mkMonthAcc(year, "", eva, loc) * 1000

    env <- data.frame(rain=dprecps, e=evaps)

    return(env)
}

mkYearE <- function(year, loc) {
    dprecps <- mkMonthAcc(year, "", tp, loc) * 1000
    dtemps <- mkMonthInst(year, "", t2m, 4, loc) - 273.15
    drads <- mkMonthAcc(year, "", ssr, loc) / 1000000

    env <- data.frame(rain=dprecps, e=revap(dtemps, drads))

    return(env)
}

posOrZero <- function(x) {
    if (x < 0.0) {
        return(0.0)
    }

    return(x)
}

mkYearSoil <- function(wsoil, prec, eva) {
    ndays <- length(prec)
    soilWater <- rep(0, ndays)
    wsoilMax <- 400
    
    for (i in 1:ndays) {
        dw <- prec[i] + eva[i]
        if (wsoil >= wsoilMax && dw > 0) {
            wsoil <- wsoil - dw
        } else {
            wsoil <- posOrZero(wsoil + dw)
        }
        soilWater[i] <- posOrZero(wsoil)
    }

    return(soilWater)

}

mkSoilWater <- function(prec, eva) {
    ndays <- length(prec)
    sw <- mkYearSoil(400, prec,eva)
    sw1 <- mkYearSoil(sw[ndays], prec,eva)
    sw2 <- mkYearSoil(sw1[ndays], prec,eva)

    return(sw2)
}

fwstom <- function(thw) {
    fw <- min(1, max(0.01, (thw-0.6)/0.2))

    return(sqrt(fw))
}
