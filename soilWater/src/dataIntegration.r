source("processWeather.r")

valInd <- c(211, 271)
oulInd <- c(421, 61)
halInd <- c(301, 181)
norInd <- c(211, 181)

hourify <- function(dailyVals) {
    hrVals <- rep(0, length(dailyVals)*24)

    for (i in 1:(length(dailyVals))) {
        dayS <- (i-1)*24+1
        dayE <- i*24
        hrVals[dayS:dayE] <- dailyVals[i]
    }

    return(hrVals)
}

indToString <- function(ind) {
    s <- paste(as.character(ind[1]), "_", as.character(ind[2]), sep="")
    return(s)
}

integrateData <- function(ind, nm) {
    hourInd <- 1
    dlenInd <- 3
    dnInd <- 4

    indStr <- indToString(ind)
    
    radFile <- paste("../../data/swrad_2010-2011_", indStr, ".csv", sep="")
    tempFile <- paste("../../data/airt_2010-2011_", indStr , ".csv", sep="")
    envFile <- paste("../../data/rad/weather", nm,  "2yrsRad.csv", sep="")

    rads <- read.table(radFile, header=T)
    temps <-  read.table(tempFile, header=T)
    envs <- read.table(envFile, sep=",", header=F)

    swInds <- mkSWIndex("20102011", "", val)

    env <- data.frame(hour = envs[, hourInd],
                      dLen = envs[, dlenInd],
                      dn = envs[, dnInd],
                      temp = temps$x,
                      moist = hourify(swInds),
                      rad = rads$x)
    return(env)
}
