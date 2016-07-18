
# Plot spatial pattern of RMSE between TELSEM and CSU for the whole domain
# for two years (CSU: time steps 2346-3075 -> 00Z01JAN2009 to 00Z31DEC2010)
# TELSEM: spline interpreted from monthly to daily 

library(fields) 

nc=20
nr=20
nch=12 # 12 channels, V, H, V, H, ... 

   to.read=file("../A.27gd4r", "rb")
   x =readBin(to.read, numeric(), n=nc*nr*28*4000, size=4, endian="big")
   close(to.read)

n1=2345 # 1aug2002 to 31dec2008
n2=3075 # 1jan2009-31dec2010 

x[x==0]=NA
vdata=array(x[(nc*nr*28*n1+1):(nc*nr*28*n2)], c(nc, nr, 28, (n2-n1+1)), dimnames=c("x", "y", "var", "t"))

vem         =vdata[ , , 16:27, ]

#vem7V         =vdata[ , , 16, ]
#vem7H         =vdata[ , , 17, ]
#vem11V        =vdata[ , , 18, ]
#vem11H        =vdata[ , , 19, ]
#vem19V        =vdata[ , , 20, ]
#vem19H        =vdata[ , , 21, ]
#vem24V        =vdata[ , , 22, ]
#vem24H        =vdata[ , , 23, ]
#vem37V        =vdata[ , , 24, ]
#vem37H        =vdata[ , , 25, ]
#vem89V        =vdata[ , , 26, ]
#vem89H        =vdata[ , , 27, ]

# reading TELSEM
   to.read2=file("/home/ytian/lswg/TELSEM/GPM/Build_Emis/SGP-emis-amsre-0.25.12gd4r", "rb")
   y =readBin(to.read2, numeric(), n=nc*nr*12*12, size=4, endian="big")
   close(to.read2)

  telsem=array(y, c(nc, nr, 12, 12), dimnames=c("x", "y", "var", "t"))

errs=array(-9999.0, dim=c(nc, nr, nch))
lon=-99.875+(1:nc)*0.25
lat=34.125+(1:nr)*0.25

for (ich in 1:nch) {
 for (ir in 1:nr ) {
  for (ic in 1:nc ) {

   xm = vem[ic, ir, ich, ]  # daily 
   y = telsem[ic, ir, ich, ]   # monthly for 1-year
   y2 = c(y, y)  # get to two years 

   #interpret ym to daily 
   ym = approx(y2,  n=length(xm))   # two year, daily 
   errs[ic, ir, ich]=sqrt(mean((xm-ym$y)^2, na.rm=T))
  }
 }
}

par(mfcol=c(2,4), mai=c(0.6, 0.3, 0.3, 0.1))
par(bg="white")
#  prediction error
image.plot(lon, lat, errs[ , , 3]*100, main="error*100 (11V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 5]*100, main="error*100 (19V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, errs[ , , 7]*100, main="error*100 (24V)", horizontal=T, zlim=c(0, 5),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 9]*100, main="error*100 (37V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 11]*100, main="error*100 (89V)", horizontal=T, zlim=c(0, 8),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

image.plot(lon, lat, errs[ , , 4]*100, main="error*100 (11H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 6]*100, main="error*100 (19H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, errs[ , , 8]*100, main="error*100 (24H)", horizontal=T, zlim=c(0, 5),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 10]*100, main="error*100 (37H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 12]*100, main="error*100 (89H)", horizontal=T, zlim=c(0, 8),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

dev.copy(postscript, "plot-spatial-TELSEM-err.ps", horizontal=T)
dev.off()

# print the numbers
#print(paste("TELSEM 10V=", round(mean(errs[ , , 3]*100), 3)), quote=F)
#print(paste("TELSEM 19V=", round(mean(errs[ , , 5]*100), 3)), quote=F)
#print(paste("TELSEM 37V=", round(mean(errs[ , , 9]*100), 3)), quote=F)
#print(paste("TELSEM 89V=", round(mean(errs[ , , 11]*100), 3)), quote=F)

#print("") 

#print(paste("TELSEM 10H=", round(mean(errs[ , , 4]*100), 3)), quote=F)
#print(paste("TELSEM 19H=", round(mean(errs[ , , 6]*100), 3)), quote=F)
#print(paste("TELSEM 37H=", round(mean(errs[ , , 10]*100), 3)), quote=F)
#print(paste("TELSEM 89H=", round(mean(errs[ , , 12]*100), 3)), quote=F)


cat("\n  Table X: Mean prediction errors\n")
cat("============================================================================\n")
cat("         |       10V     10H     19V     19H     37V     37H     89V     89H\n")
cat("----------------------------------------------------------------------------\n")
cat(paste("Clim:  |",
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 5]*100), 2),
            round(mean(errs[ , , 6]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), 
            round(mean(errs[ , , 11]*100), 2),
            round(mean(errs[ , , 12]*100), 2), 
           "\n", sep='\t' ) )
cat("============================================================================\n")


