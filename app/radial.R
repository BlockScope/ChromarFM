library("plotrix")
library("colorspace")

mkRadialPlot <- function(fn, title) {

    d <- read.csv(fn)

    ticks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) * pi/6
    months = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

    m <- data.matrix(d, rownames.force=FALSE)[, 2:4]
    radial.plot(lengths=t(m), radial.pos=d$time * 2 * pi / 365, clockwise=TRUE, rp.type="p", lwd=4,  line.col=rainbow_hcl(3), 
                labels= months, label.pos = ticks, main=title, show.grid.labels=FALSE)
    
}

par(mfrow=c(2, 4))
mkRadialPlot("../out/lifeExpsOul/outNsHYear.csv", "Oulu - High Dorm")
mkRadialPlot("../out/lifeExpsHal/outNsHYear.csv", "Halle - High Dorm")
mkRadialPlot("../out/lifeExpsNor/outNsHYear.csv", "Norwich - High Dorm")
mkRadialPlot("../out/lifeExpsVal/outNsHYear.csv", "Valencia - High Dorm")

mkRadialPlot("../out/lifeExpsOul/outNsLYear.csv", "Oulu - Low Dorm")
mkRadialPlot("../out/lifeExpsHal/outNsLYear.csv", "Halle - Low Dorm")
mkRadialPlot("../out/lifeExpsNor/outNsLYear.csv", "Norwich - Low Dorm")
mkRadialPlot("../out/lifeExpsVal/outNsLYear.csv", "Valencia - Low Dorm")
legend("bottomright", c("seeds", "plants", "fplants"), col=rainbow_hcl(3), pch=15)

