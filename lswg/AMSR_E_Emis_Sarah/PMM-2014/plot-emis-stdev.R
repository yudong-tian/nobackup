

# Plot spatial pattern of the mean emissivity  for the whole domain
# for the validation period (t=2346~3075) (00Z01JAN2009 to 00Z31DEC2010) 

library(fields) 

# plot spatial pattern of emissivity
plot_emis <- function(lon, lat, emis, method) {

par(mfcol=c(2,4), mai=c(0.6, 0.3, 0.3, 0.1))
par(bg="white")

#  mean emis
image.plot(lon, lat, emis[ , , 1], main="11V", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, emis[ , , 3], main="19V", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, emis[ , , 7], main="24V", horizontal=T, zlim=c(0, 8),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, emis[ , , 7], main="37V", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, emis[ , , 9], main="89V", horizontal=T, zlim=c(0, 8),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

image.plot(lon, lat, emis[ , , 2], main="11H", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, emis[ , , 4], main="19H", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, emis[ , , 8], main="24H", horizontal=T, zlim=c(0, 8),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, emis[ , , 8], main="37H", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, emis[ , , 10], main="89H", horizontal=T, zlim=c(0, 8),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

dev.copy(postscript, paste(method, "-spatial-emis.ps", sep=""), horizontal=T)
dev.off()

}

nc=20
nr=20
nch=10 # 10 channels, from 11, 19, 24, 37, 89G, (V, H, V, H, ... ) 

lon=-99.875+(1:nc)*0.25
lat=34.125+(1:nr)*0.25


   to.read=file("../A.27gd4r", "rb")
   x =readBin(to.read, numeric(), n=nc*nr*28*4000, size=4, endian="big")
   close(to.read)

n1=2345 # 1aug2002 to 31dec2008
n2=3075 # 1jan2009-31dec2010 

x[x==0]=NA
data=array(x[1:(nc*nr*28*n1)], c(nc, nr, 28, n1), dimnames=c("x", "y", "var", "t"))
vdata=array(x[(nc*nr*28*n1+1):(nc*nr*28*n2)], c(nc, nr, 28, (n2-n1+1)), dimnames=c("x", "y", "var", "t"))

tbs = data[, , 6:15, ]
# 25 predictors 
tbV=tbs[, , 0:4*2+1, ]
tbH=tbs[, , 1:5*2, ]
tbV2=tbV**2 
tbH2=tbH**2 
mpdi=(tbV-tbH)/(tbV+tbH) 
mpdi2=mpdi**2 

vtbs = vdata[, , 6:15, ]
# 25 predictors 
vtbV=vtbs[, , 0:4*2+1, ]
vtbH=vtbs[, , 1:5*2, ]
vtbV2=vtbV**2 
vtbH2=vtbH**2 
vmpdi=(vtbV-vtbH)/(vtbV+vtbH) 
vmpdi2=vmpdi**2 

ems = data[, , 18:27, ]
vems = vdata[, , 18:27, ]

avg=array(-9999.0, dim=c(nc, nr, nch))
for (ich in 1:nch) { 
 for (ir in 1:nr ) {
  for (ic in 1:nc ) {

   avg[ic, ir, ich]=sd(vems[ic, ir, ich, ], na.rm=T)*100      

  }
 }
}

plot_emis(lon, lat, avg, "stdev")

