library("plotrix")
library("colorspace")

mkRadialPlot <- function(fn, title) {

    d <- read.csv(fn)

    ticks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) * pi/6
    months = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

    m <- data.matrix(d, rownames.force=FALSE)[, 2:4]
    radial.plot(lengths=t(m), radial.pos=d$time * 2 * pi / 365, clockwise=TRUE, rp.type="p", lwd=4,  line.col=rainbow_hcl(3), 
                labels= months, label.pos = ticks, main=title, show.grid.labels=FALSE)

    legend("bottomright", c("seeds", "plants", "fplants"), col=rainbow_hcl(3), pch=15)
}

filenames = c("../out/lifeExpsVal/outEvents_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsVal/outEvents_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsVal/outEvents_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsVal/outEvents_d2.5_r0.737AvgYear.txt",
    "../out/lifeExpsOul/outEvents_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsOul/outEvents_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsOul/outEvents_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsOul/outEvents_d2.5_r0.737AvgYear.txt",
    "../out/lifeExpsHal/outEvents_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsHal/outEvents_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsHal/outEvents_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsHal/outEvents_d2.5_r0.737AvgYear.txt",
    "../out/lifeExpsNor/outEvents_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsNor/outEvents_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsNor/outEvents_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsNor/outEvents_d2.5_r0.737AvgYear.txt")

filenamesH = c("../out/lifeExpsVal/outEventsH_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsVal/outEventsH_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsVal/outEventsH_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsVal/outEventsH_d2.5_r0.737AvgYear.txt",
    "../out/lifeExpsOul/outEventsH_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsOul/outEventsH_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsOul/outEventsH_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsOul/outEventsH_d2.5_r0.737AvgYear.txt",
    "../out/lifeExpsHal/outEventsH_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsHal/outEventsH_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsHal/outEventsH_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsHal/outEventsH_d2.5_r0.737AvgYear.txt",
    "../out/lifeExpsNor/outEventsH_d0.0_r0.598AvgYear.txt",
    "../out/lifeExpsNor/outEventsH_d0.0_r0.737AvgYear.txt",
    "../out/lifeExpsNor/outEventsH_d2.5_r0.598AvgYear.txt",
    "../out/lifeExpsNor/outEventsH_d2.5_r0.737AvgYear.txt")

titles = c(
    "(Valencia,(0.0,0.598))",
    "(Valencia,(0.0,0.737))",
    "(Valencia,(2.5,0.598))",
    "(Valencia,(2.5,0.737))",
    "(Oulu,(0.0,0.598))",
    "(Oulu,(0.0,0.737))",
    "(Oulu,(2.5,0.598))",
    "(Oulu,(2.5,0.737))",
    "(Halle,(0.0,0.598))",
    "(Halle,(0.0,0.737))",
    "(Halle,(2.5,0.598))",
    "(Halle,(2.5,0.737))",
    "(Norwich,(0.0,0.598))",
    "(Norwich,(0.0,0.737))",
    "(Norwich,(2.5,0.598))",
    "(Norwich,(2.5,0.737))")

fouts = c(
    "../out/lifeExpsVal/plots/timingsd0.0_r0.598.png",
    "../out/lifeExpsVal/plots/timingsd0.0_r0.737.png",
    "../out/lifeExpsVal/plots/timingsd2.5_r0.598.png",
    "../out/lifeExpsVal/plots/timingsd2.5_r0.737.png",
    "../out/lifeExpsOul/plots/timingsd0.0_r0.598.png",
    "../out/lifeExpsOul/plots/timingsd0.0_r0.737.png",
    "../out/lifeExpsOul/plots/timingsd2.5_r0.598.png",
    "../out/lifeExpsOul/plots/timingsd2.5_r0.737.png",
    "../out/lifeExpsHal/plots/timingsd0.0_r0.598.png",
    "../out/lifeExpsHal/plots/timingsd0.0_r0.737.png",
    "../out/lifeExpsHal/plots/timingsd2.5_r0.598.png",
    "../out/lifeExpsHal/plots/timingsd2.5_r0.737.png",
    "../out/lifeExpsNor/plots/timingsd0.0_r0.598.png",
    "../out/lifeExpsNor/plots/timingsd0.0_r0.737.png",
    "../out/lifeExpsNor/plots/timingsd2.5_r0.598.png",
    "../out/lifeExpsNor/plots/timingsd2.5_r0.737.png")

foutsH = c(
    "../out/lifeExpsVal/plots/timingsHd0.0_r0.598.png",
    "../out/lifeExpsVal/plots/timingsHd0.0_r0.737.png",
    "../out/lifeExpsVal/plots/timingsHd2.5_r0.598.png",
    "../out/lifeExpsVal/plots/timingsHd2.5_r0.737.png",
    "../out/lifeExpsOul/plots/timingsHd0.0_r0.598.png",
    "../out/lifeExpsOul/plots/timingsHd0.0_r0.737.png",
    "../out/lifeExpsOul/plots/timingsHd2.5_r0.598.png",
    "../out/lifeExpsOul/plots/timingsHd2.5_r0.737.png",
    "../out/lifeExpsHal/plots/timingsHd0.0_r0.598.png",
    "../out/lifeExpsHal/plots/timingsHd0.0_r0.737.png",
    "../out/lifeExpsHal/plots/timingsHd2.5_r0.598.png",
    "../out/lifeExpsHal/plots/timingsHd2.5_r0.737.png",
    "../out/lifeExpsNor/plots/timingsHd0.0_r0.598.png",
    "../out/lifeExpsNor/plots/timingsHd0.0_r0.737.png",
    "../out/lifeExpsNor/plots/timingsHd2.5_r0.598.png",
    "../out/lifeExpsNor/plots/timingsHd2.5_r0.737.png")


for (i in 1:length(titles)) {
    png(fouts[i])
    mkRadialPlot(fn=filenames[i], titles[i])
    dev.off()
    
}

for (i in 1:length(titles)) {
    png(foutsH[i])
    mkRadialPlot(fn=filenamesH[i], titles[i])
    dev.off()
}
