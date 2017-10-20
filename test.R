# test voronoi

# deldir example
library(deldir)
x <- 1000*runif(10)
y <- 1000*runif(10)
vt <- deldir(x, y)
oldMar = par()$mar
par(mar=rep(1, 4))
plot(x, y, axes=FALSE, ann=FALSE, pch=16)
plot(vt, wlines="tess", lty="solid", add=TRUE)
box()
par(mar=oldMar)

# source stuff
scale <- 1000
setwd("/Users/slwu89/Desktop/git/voronoi")
source("allocate.R")
source("debug.R")
source("draw.R")
source("soiltexture_functions.R")
source("utility.R")
source("awv.R")

w <- runif(10, 1, 100)
library(gpclib)
unitSquare <- as(list(x=c(0, 0, 1000, 1000, 0),
                      y=c(0, 1000, 1000, 0, 0)),
                 "gpc.poly")
awvt <- awv(list(x=x, y=y), w, unitSquare)
oldMar = par()$mar
par(mar=rep(1, 4))
plot(x, y, axes=FALSE, ann=FALSE, pch=16)
lapply(awvt, plot, add=TRUE)
text(x, y, round(w), pos=3)
box()
par(mar=oldMar)

temp <- seq(.1, .9, length=10)
target <- temp/sum(temp)
treemap <- allocate(letters[1:10], 
                      list(x=x, y=y), 
                      w, unitSquare, target,debug = TRUE)
drawRegions(treemap, label=TRUE)

area <- unlist(treemap$a)
temp <- rbind(100*round(area/sum(area), 3),
              100*round(target, 3))
colnames(temp) <- letters[1:10]
capture.output(temp)