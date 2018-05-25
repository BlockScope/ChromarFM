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

integrateData <- function(nm) {
    hourInd <- 1
    dlenInd <- 3
    dnInd <- 4
    tempInd <- 5
    radInd <- 7

    envFile <- paste("../../data/rad/weather", nm,  "2yrsRad.csv", sep="")

    envs <- read.table(envFile, sep=",", header=F)

    swInds <- mkSWIndex("20102011", "", oul)

    env <- data.frame(hour = envs[, hourInd],
                      dLen = envs[, dlenInd],
                      dn = envs[, dnInd],
                      temp = envs[,tempInd],
                      moist = hourify(swInds),
                      rad = envs[, radInd])
    return(env)
}
