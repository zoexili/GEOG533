# lab 4
# Question 1 
## a
pnorm(20,32,7)

curve(dnorm(x,32,7),from=11,to=53)
cord.x <- c(11,seq(11,20,0.01),20)
cord.y <- c(0,dnorm(seq(11,20,0.01),32,7),0)
polygon(cord.x,cord.y,col='red')

## b
pnorm(35,32,7,lower.tail = FALSE)
cord.x1 <- c(35,seq(35,53,0.01),53)
cord.y1 <- c(0,dnorm(seq(35,53,0.01),32,7),0)
polygon(cord.x1,cord.y1,col='skyblue')

## c
pnorm(40,32,7) - pnorm(20,32,7) 
cord.x2 <- c(20,seq(20,40,0.01),40)
cord.y2 <- c(0,dnorm(seq(20,40,0.01),32,7),0)
polygon(cord.x2,cord.y2,col="purple")

# Question 2
## a
pexp(3,rate=0.1)
curve(dexp(x,0.1),xlim=c(0,5),col='green')
pnt.x <- c(0,seq(0,3,0.01),3)
pnt.y <- c(0,dexp(seq(0,3,0.01),0.1),0)
polygon(pnt.x,pnt.y,col='skyblue')

## b 
pexp(20,rate=0.1,lower.tail=FALSE)
curve(dexp(x,0.1),xlim=c(20,100))
pnt.x1 <- c(20,seq(20,100,0.01),100)
pnt.x2 <- c(0,dexp(seq(20,100,0.01),0.1),0)
polygon(pnt.x1,pnt.x2,col='red')

## c
pexp(10,rate=0.1) - pexp(5,rate=0.1)
curve(dexp(x,0.1),xlim=c(3,12))
pnt.x2 <- c(5,seq(5,10,0.01),10)
pnt.y2 <- c(0,dexp(seq(5,10,0.01),0.1),0)
polygon(pnt.x2,pnt.y2,col='purple')

# Question 3
## a
install.packages("raster")
library(raster)
ras <- raster(nrows=30,ncols=30,xmn=0,xmx=10,ymn=0,ymx=10)
set.seed(0)
ras[] <- runif(ncell(ras))
plot(ras)
hist(ras)

## b
ras2 <- raster(nrows=30,ncols=30,xmn=0,xmx=10,ymn=0,ymx=10)
set.seed(1)
ras2[] <- rnorm(ncell(ras2))
plot(ras2)
hist(ras2)

## c
ras3 <- raster(nrows=30,ncols=30,xmn=0,xmx=10,ymn=0,ymx=10)
ras3 <- ras+ras2
plot(ras3)
hist(ras3)

## d
##### mean value
cellStats(ras3,"mean")

##### reclassify

ras3[ras3 > 0.4891908] <- 1
ras3[ras3 < 0.4891908] <- 0
ras3[ras3 == 0.4891908] <- 0
plot(ras3)
hist(ras3)

library(rgdal)
writeRaster(ras3,filename="reclassify.tif")
