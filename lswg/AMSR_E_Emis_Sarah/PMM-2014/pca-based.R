
# implementing pca based ... not done yet

# Use 25 predictors as Yalei suggested
# Tb_V (5 freqs), Tb_H(5), Tb_V^2 (5), Tb_H^2 (5), and MPDI (5) 

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
nch=10 # 10 channels, from 11, 19, 24, 37, 89G, (V, H, V, H, ... ) 

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

as=array(-9999.0, dim=c(nc, nr, nch))
bs=array(-9999.0, dim=c(nc, nr, nch))
cs=array(-9999.0, dim=c(nc, nr, nch))
errs=array(-9999.0, dim=c(nc, nr, nch))
lon=-99.875+(1:nc)*0.25
lat=34.125+(1:nr)*0.25

for (ich in 1:nch) { 
 for (ir in 1:nr ) {
  for (ic in 1:nc ) {

   # all 5 freqs  in x
   xtbV =   tbV[ic, ir, , ]
   xtbH =   tbH[ic, ir, , ]
   xtbV2 = tbV2[ic, ir, , ]
   xtbH2 = tbH2[ic, ir, , ]
   xmpdi = mpdi[ic, ir, , ]
   xmpdi2 = mpdi2[ic, ir, , ]
   
   x=t(rbind(xtbV, xtbH, xtbV2, xtbH2, xmpdi) )

   # one freq in y
   yem = ems[ic, ir, ich, ]
   avg=mean(yem, na.rm=T) 
   yem= yem - avg  # anomalies
   
   model = lm(yem ~ x)  

   # prediction 
   vxtbV =   vtbV[ic, ir, , ]
   vxtbH =   vtbH[ic, ir, , ]
   vxtbV2 = vtbV2[ic, ir, , ]
   vxtbH2 = vtbH2[ic, ir, , ]
   vxmpdi = vmpdi[ic, ir, , ]
   vxmpdi2 = vmpdi2[ic, ir, , ]

   vx=t( rbind(vxtbV, vxtbH, vxtbV2, vxtbH2, vxmpdi) ) 

   errs[ic, ir, ich]=sqrt( mean( (vems[ic, ir, ich, ]
                                  -avg-predict(model, list(x=vx)) )^2, na.rm=T ) )

  }
 }
}

par(mfcol=c(2,4), mai=c(0.6, 0.3, 0.3, 0.1))
par(bg="white")

#  prediction error 
image.plot(lon, lat, errs[ , , 1]*100, main="error*100 (11V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 3]*100, main="error*100 (19V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, errs[ , , 7]*100, main="error*100 (24V)", horizontal=T, zlim=c(0, 5),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 7]*100, main="error*100 (37V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 9]*100, main="error*100 (89V)", horizontal=T, zlim=c(0, 8),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

image.plot(lon, lat, errs[ , , 2]*100, main="error*100 (11H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 4]*100, main="error*100 (19H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, errs[ , , 8]*100, main="error*100 (24H)", horizontal=T, zlim=c(0, 5),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 8]*100, main="error*100 (37H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 10]*100, main="error*100 (89H)", horizontal=T, zlim=c(0, 8), 
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

dev.copy(postscript, "plot-pca-based.ps", horizontal=T)
dev.off()

# print the numbers
print(paste("MPDI-based 10V=", round(mean(errs[ , , 1]*100), 3)), quote=F)
print(paste("MPDI-based 19V=", round(mean(errs[ , , 3]*100), 3)), quote=F)
print(paste("MPDI-based 37V=", round(mean(errs[ , , 7]*100), 3)), quote=F)
print(paste("MPDI-based 89V=", round(mean(errs[ , , 9]*100), 3)), quote=F)

print("")

print(paste("MPDI-based 10H=", round(mean(errs[ , , 2]*100), 3)), quote=F)
print(paste("MPDI-based 19H=", round(mean(errs[ , , 4]*100), 3)), quote=F)
print(paste("MPDI-based 37H=", round(mean(errs[ , , 8]*100), 3)), quote=F)
print(paste("MPDI-based 89H=", round(mean(errs[ , , 10]*100), 3)), quote=F)


