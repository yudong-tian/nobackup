
# Plot spatial pattern of the parameters and errors for the whole domain
# using the first 3000 time-steps of data for model fitting, 
# the rest 1000 time-steps for validation

library(fields) 

fit_only <- function (xm, ym, vxm, vym) { 
   out = nls( ym ~ a + b * xm + c * xm * xm, start=list(a=1, b=1, c=0),
          control=nls.control(minFactor=1/4096, warnOnly = T))
   vx = vxm[!is.na(vxm)]
   vy = vym[!is.na(vym)]

   abc=coef(out)
   err=sd(vy-predict(out, list(xm=vx)))
   return(list(a=abc[1], b=abc[2], c=abc[3], err=err)) 

}


nc=20
nr=20
nch=12 # 12 channels, V, H, V, H, ... 

   to.read=file("../A.27gd4r", "rb")
   x =readBin(to.read, numeric(), n=nc*nr*28*4000, size=4, endian="big")
   close(to.read)

n1=2588 # 1aug2002 to 31aug2009
n2=3287 # 31jul2011

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

mpdi= (em11V - em11H)/(em11V + em11H)
vmpdi= (vem11V - vem11H)/(vem11V + vem11H)
  
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


#  prediction error 
image.plot(lon, lat, errs[ , , 3]*100, main="error*100 (11V", horizontal=T, zlim=c(0, 5))
image.plot(lon, lat, errs[ , , 7]*100, main="error*100 (24V", horizontal=T, zlim=c(0, 5))
image.plot(lon, lat, errs[ , , 9]*100, main="error*100 (37V", horizontal=T, zlim=c(0, 5))
image.plot(lon, lat, errs[ , , 11]*100, main="error*100 (89V", horizontal=T, zlim=c(0, 8))

image.plot(lon, lat, errs[ , , 4]*100, main="error*100 (11H)", horizontal=T, zlim=c(0, 5))
image.plot(lon, lat, errs[ , , 8]*100, main="error*100 (24H)", horizontal=T, zlim=c(0, 5))
image.plot(lon, lat, errs[ , , 10]*100, main="error*100 (37H)", horizontal=T, zlim=c(0, 5))
image.plot(lon, lat, errs[ , , 12]*100, main="error*100 (89H)", horizontal=T, zlim=c(0, 8), 
         axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" ) 

dev.copy(postscript, "plot-spatial-parm-err.ps", horizontal=T)
dev.off()







