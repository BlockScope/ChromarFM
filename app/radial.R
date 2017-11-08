library("plotrix")
library("colorspace")

d <- read.csv("../out/lifeExpsVal/outNsYear.txt")

ticks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) * pi/6
months = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

m <- data.matrix(d, rownames.force=FALSE)[, 2:4]
radial.plot(lengths=t(m), radial.pos=d$time * 2 * pi / 365, clockwise=TRUE, rp.type="p", lwd=3,  line.col=rainbow_hcl(3), 
            labels= months, label.pos = ticks, main="Valencia, frepr=L, dorm=L")
legend("bottomright", c("seeds", "plants", "fplants"), col=rainbow_hcl(3), pch=15)
