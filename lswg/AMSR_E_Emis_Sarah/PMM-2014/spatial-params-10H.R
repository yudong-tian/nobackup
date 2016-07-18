
# Use 10H emissivity as predictor, for comparison with using MPDI as predictor
#  The former approach is used by Bytheway and Kummerow (2010).

# Plot spatial pattern of the parameters and errors for the whole domain
# using the first 2345 (00Z01AUG2002 to 00Z31DEC2008) time-steps of data for model fitting,
# the 2346-3075 (00Z01JAN2009 to 00Z31DEC2010) time-steps for validation

library(fields) 

fit_only <- function (xm, ym, vxm, vym) { 
   out = nls( ym ~ a + b * xm + c * xm * xm, start=list(a=1, b=1, c=0),
          control=nls.control(minFactor=1/4096, warnOnly = T))
   vx = vxm[!is.na(vxm)]
   vy = vym[!is.na(vym)]

   abc=coef(out)
   #err=sd(vy-predict(out, list(xm=vx)))
   err=sqrt( mean( (vy-predict(out, list(xm=vx)) )^2 ))
   return(list(a=abc[1], b=abc[2], c=abc[3], err=err)) 

}


nc=20
nr=20
nch=12 # 12 channels, V, H, V, H, ... 

   to.read=file("../A.27gd4r", "rb")
   x =readBin(to.read, numeric(), n=nc*nr*28*4000, size=4, endian="big")
   close(to.read)

#n1=2588 # 1aug2002 to 31aug2009
#n2=3287 # 31jul2011

n1=2345 # 1aug2002 to 31dec2008
n2=3075 # 1jan2009-31dec2010

x[x==0]=NA
data=array(x[1:(nc*nr*28*n1)], c(nc, nr, 28, n1), dimnames=c("x", "y", "var", "t"))
vdata=array(x[(nc*nr*28*n1+1):(nc*nr*28*n2)], c(nc, nr, 28, (n2-n1+1)), dimnames=c("x", "y", "var", "t"))

em7V         =data[ , , 16, ]
em7H         =data[ , , 17, ]
em11V        =data[ , , 18, ]
em11H        =data[ , , 19, ]
em19V        =data[ , , 20, ]
em19H        =data[ , , 21, ]
em24V        =data[ , , 22, ]
em24H        =data[ , , 23, ]
em37V        =data[ , , 24, ]
em37H        =data[ , , 25, ]
em89V        =data[ , , 26, ]
em89H        =data[ , , 27, ]

vem7V         =vdata[ , , 16, ]
vem7H         =vdata[ , , 17, ]
vem11V        =vdata[ , , 18, ]
vem11H        =vdata[ , , 19, ]
vem19V        =vdata[ , , 20, ]
vem19H        =vdata[ , , 21, ]
vem24V        =vdata[ , , 22, ]
vem24H        =vdata[ , , 23, ]
vem37V        =vdata[ , , 24, ]
vem37H        =vdata[ , , 25, ]
vem89V        =vdata[ , , 26, ]
vem89H        =vdata[ , , 27, ]

#mpdi= (em11V - em11H)/(em11V + em11H)
#vmpdi= (vem11V - vem11H)/(vem11V + vem11H)
# Bytheway and Kummerow (2010)
mpdi= em11H
vmpdi= vem11H
  
as=array(-9999.0, dim=c(nc, nr, nch))
bs=array(-9999.0, dim=c(nc, nr, nch))
cs=array(-9999.0, dim=c(nc, nr, nch))
errs=array(-9999.0, dim=c(nc, nr, nch))
lon=-99.875+(1:nc)*0.25
lat=34.125+(1:nr)*0.25

for (ich in 1:nch) { 
 for (ir in 1:nr ) {
  for (ic in 1:nc ) {

   # 11H as function of mpdi at (1, 1) and (10, 10)
   xm = mpdi[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   ym = data[ic, ir, 15+ich, ]
   vym = vdata[ic, ir, 15+ich, ]
   res=fit_only(xm, ym, vxm, vym) 
   as[ic, ir, ich]=res$a
   bs[ic, ir, ich]=res$b
   cs[ic, ir, ich]=res$c
   errs[ic, ir, ich]=res$err
  }
 }
}

par(mfcol=c(2,4), mai=c(0.6, 0.3, 0.3, 0.1))
par(bg="white")

#  parameter a
image.plot(lon, lat, as[ , , 3], main="a (11V)", horizontal=T) 
image.plot(lon, lat, as[ , , 5], main="a (19V)", horizontal=T) 
#image.plot(lon, lat, as[ , , 7], main="a (24V)", horizontal=T) 
image.plot(lon, lat, as[ , , 9], main="a (37V)", horizontal=T) 
image.plot(lon, lat, as[ , , 11], main="a (89V)", horizontal=T) 

image.plot(lon, lat, as[ , , 4], main="a (11H)", horizontal=T) 
image.plot(lon, lat, as[ , , 6], main="a (19H)", horizontal=T) 
#image.plot(lon, lat, as[ , , 8], main="a (24H)", horizontal=T) 
image.plot(lon, lat, as[ , , 10], main="a (37H)", horizontal=T) 
image.plot(lon, lat, as[ , , 12], main="a (89H)", horizontal=T) 

dev.copy(postscript, "plot-spatial-parm-a-10H.ps", horizontal=T)
dev.off()

#  parameter b
image.plot(lon, lat, bs[ , , 3], main="b (11V)", horizontal=T)
image.plot(lon, lat, bs[ , , 5], main="b (19V)", horizontal=T)
#image.plot(lon, lat, bs[ , , 7], main="b (24V)", horizontal=T)
image.plot(lon, lat, bs[ , , 9], main="b (37V)", horizontal=T)
image.plot(lon, lat, bs[ , , 11], main="b (89V)", horizontal=T)

image.plot(lon, lat, bs[ , , 4], main="b (11H)", horizontal=T)
image.plot(lon, lat, bs[ , , 6], main="b (19H)", horizontal=T)
#image.plot(lon, lat, bs[ , , 8], main="b (24H)", horizontal=T)
image.plot(lon, lat, bs[ , , 10], main="b (37H)", horizontal=T)
image.plot(lon, lat, bs[ , , 12], main="b (89H)", horizontal=T)

dev.copy(postscript, "plot-spatial-parm-b-10H.ps", horizontal=T)
dev.off()

#  parameter c
image.plot(lon, lat, cs[ , , 3], main="c (11V)", horizontal=T)
image.plot(lon, lat, cs[ , , 5], main="c (19V)", horizontal=T)
#image.plot(lon, lat, cs[ , , 7], main="c (24V)", horizontal=T)
image.plot(lon, lat, cs[ , , 9], main="c (37V)", horizontal=T)
image.plot(lon, lat, cs[ , , 11], main="c (89V)", horizontal=T)

image.plot(lon, lat, cs[ , , 4], main="c (11H)", horizontal=T)
image.plot(lon, lat, cs[ , , 6], main="c (19H)", horizontal=T)
#image.plot(lon, lat, cs[ , , 8], main="c (24H)", horizontal=T)
image.plot(lon, lat, cs[ , , 10], main="c (37H)", horizontal=T)
image.plot(lon, lat, cs[ , , 12], main="c (89H)", horizontal=T)

dev.copy(postscript, "plot-spatial-parm-c-10H.ps", horizontal=T)
dev.off()

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

dev.copy(postscript, "plot-spatial-parm-err-10H.ps", horizontal=T)
dev.off()

par(mfrow=c(1,1)) 
par(bg="white")

# plot b-c relationship 
plot(bs[ , , 12], cs[ , , 12], xlab="b", ylab="c")
points(bs[, , 11], cs[, , 11], col="red")
points(bs[, , 10], cs[, , 10], col="green")
points(bs[, , 9], cs[, , 9], col="blue")
points(bs[, , 8], cs[, , 8], col="orange")
points(bs[, , 7], cs[, , 7], col="pink")

dev.copy(postscript, "plot-b-c-corrr-10H.ps", horizontal=T)
dev.off()

# print the numbers
print(paste("10H-based 10V=", round(mean(errs[ , , 3]*100), 3)), quote=F)
print(paste("10H-based 19V=", round(mean(errs[ , , 5]*100), 3)), quote=F)
print(paste("10H-based 37V=", round(mean(errs[ , , 9]*100), 3)), quote=F)
print(paste("10H-based 89V=", round(mean(errs[ , , 11]*100), 3)), quote=F)

print("")

print(paste("10H-based 10H=", round(mean(errs[ , , 4]*100), 3)), quote=F)
print(paste("10H-based 19H=", round(mean(errs[ , , 6]*100), 3)), quote=F)
print(paste("10H-based 37H=", round(mean(errs[ , , 10]*100), 3)), quote=F)
print(paste("10H-based 89H=", round(mean(errs[ , , 12]*100), 3)), quote=F)








