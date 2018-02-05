library("ggplot2")
library("cowplot")
source("processWeather.r")
source("soilModel.r")

plotMoistIndex <- function(year, month, loc, name) {
    #swInd <- mkSWIndex1(year, month, loc)
    swInd <- mkSWIndex3(year, month, loc)

    swInd[swInd > 1] <- 1
    p <- ggplot() + geom_line(aes(x=1:(length(swInd)), y=swInd), size=1.0, colour="blue", alpha=0.7)
    p <- p + labs(x="day year", y="moist index", title=name)
    p <- p + ylim(0, 1)
    return(p)
}

goMoistIndexes <- function() {
    pVal <- plotMoistIndex("20102011", "", val, "Valencia")
    pHal <- plotMoistIndex("20102011", "", hal, "Halle")
    pOul <- plotMoistIndex("20102011", "", oul, "Oulu")
    pEdin <- plotMoistIndex("20102011", "", edin, "Edinburgh")

    plot_grid(pVal, pHal, pOul, pEdin)
}

plotMoistIndexModel <- function(year, month, loc, name) {
    dates <- seq(from=as.POSIXct("2010-01-01"), to=as.POSIXct("2011-12-31"), by="days")
    env <- processWeather("20102011", "", loc, dates)

    swInd <- mkSoilWater(env) / 400.0
    swInd[swInd > 1] <- 1
    p <- ggplot() + geom_line(aes(x=1:(length(swInd)), y=swInd), size=1.0, colour="blue", alpha=0.7)
    p <- p + labs(x="day year", y="moist index", title=name)
    p <- p + ylim(0, 1)
    return(p)
}

goMoistIndexesModel <- function() {
   pVal <- plotMoistIndexModel("20102011", "", val, "Valencia")
   pHal <- plotMoistIndexModel("20102011", "", hal, "Halle")
   pOul <- plotMoistIndexModel("20102011", "", oul, "Oulu")
   pEdin <- plotMoistIndexModel("20102011", "", edin, "Edinburgh")

   plot_grid(pVal, pHal, pOul, pEdin)
}




