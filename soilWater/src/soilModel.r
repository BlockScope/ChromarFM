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

rhoSat <- function() {
    x <- c(-5, 0, 5, 10, 15, 20, 25, 30, 35, 40)
    y <- c(3.7, 4.9, 6.8, 9.4, 12.8, 17.3, 23.1, 30.4, 39.7, 51.2)

    return(approxfun(x, y))
}

posorzero <- function(x) {
    if (x < 0.0) {
        return(0.0)
    }

    return(x)
}

wvp <- function(temp) {
    #temp is in Kelvin
    temp <- temp + 273.15
    t0 <- 273.15
    
    a1 <- 611.21
    a3 <- 17.502
    a4 <- 32.19
    svp <- a1 * exp(a3 * (temp - t0)/(temp-a4))

    return(svp)
}

rh <- function(tempAir, dewTemp) {
    return(wvp(dewTemp) / wvp(tempAir))
}

revap <- function(temp, rad) {
    lwt <- lw()(temp)
    gt <- gm()(temp)
    st <- s()(temp)
        
    revap <- rad / (lwt * (1 + (gt / st)))
    
    return(revap*(-1))
}

resAir <- function(h, k) {
    z <- 0.01
    return((log(h/z)**2) / (k**2*0.3))
}

wvEvap <- function(temp, dtemp, d) {
    rAir <-  resAir(5, 0.4)
    rhos <- rhoSat()(temp)
    st <- s()(temp)
    gt <- gm()(temp)

    
    ewv <- 86400*d*(rhos * (1-rh(temp, dtemp))) / (rAir * (1 + (st / gt)))

    return(ewv * (-1))
}

mkYearSoil <- function(wsoil, env) {
    ndays <- length(env$rad)
    soilWater <- rep(0, ndays)
    wsoilMax <- 400
    
    for (i in 1:ndays) {
        hdrys <- (wsoilMax - wsoil) / 1000
        fssev <- 0.02 / (0.02 + hdrys)

        evapTotal <- revap(env$temps[i], env$rads[i]) +
                     wvEvap(env$temps[i], env$dtemps[i], env$ds[i])
        dw <- env$precs[i] + (evapTotal * fssev)
        if (wsoil >= wsoilMax && dw > 0) {
            wsoil <- wsoil - dw
        } else {
            wsoil <- posorzero(wsoil + dw)
        }
        soilWater[i] <- posorzero(wsoil)
    }

    return(soilWater)
}

mkSoilWater <- function(env) {
    ndays <- length(env$prec)
    sw <- mkYearSoil(400, env)
    sw1 <- mkYearSoil(sw[ndays], env)
    sw2 <- mkYearSoil(sw1[ndays], env)

    return(sw2)
}

fwstom <- function(thw) {
    fw <- min(1, max(0.01, (thw-0.6)/0.2))

    return(sqrt(fw))
}
