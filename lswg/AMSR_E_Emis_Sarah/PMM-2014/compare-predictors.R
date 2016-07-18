
# Try all kinds of predictors permutated from the following set:   
# Tb_V (5 freqs), Tb_H(5), Tb_V^2 (5), Tb_H^2 (5), and MPDI (5) 

# Plot spatial pattern of the parameters and errors for the whole domain
# using the first 2345 (00Z01AUG2002 to 00Z31DEC2008) time-steps of data for model fitting, 
# the 2346-3075 (00Z01JAN2009 to 00Z31DEC2010) time-steps for validation

library(fields) 

plot_err <- function(lon, lat, errs, method) {

par(mfcol=c(2,4), mai=c(0.6, 0.3, 0.3, 0.1))
par(bg="white")

#  prediction error
image.plot(lon, lat, errs[ , , 1], main="error*100 (11V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 3], main="error*100 (19V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, errs[ , , 7], main="error*100 (24V)", horizontal=T, zlim=c(0, 5),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 7], main="error*100 (37V)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 9], main="error*100 (89V)", horizontal=T, zlim=c(0, 8),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

image.plot(lon, lat, errs[ , , 2], main="error*100 (11H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 4], main="error*100 (19H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
#image.plot(lon, lat, errs[ , , 8], main="error*100 (24H)", horizontal=T, zlim=c(0, 5),
# axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 8], main="error*100 (37H)", horizontal=T, zlim=c(0, 5),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )
image.plot(lon, lat, errs[ , , 10], main="error*100 (89H)", horizontal=T, zlim=c(0, 8),
 axis.args=list(cex.axis=2), cex.axis=2, cex.main=2, xlab="", ylab="" )

dev.copy(postscript, paste(method, "-spatial-err.ps", sep=""), horizontal=T)
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

errs1=array(-9999.0, dim=c(nc, nr, nch))
errs2=array(-9999.0, dim=c(nc, nr, nch))
errs3=array(-9999.0, dim=c(nc, nr, nch))
errs4=array(-9999.0, dim=c(nc, nr, nch))
errs5=array(-9999.0, dim=c(nc, nr, nch))
for (ich in 1:nch) { 
 for (ir in 1:nr ) {
  for (ic in 1:nc ) {

   # regression predictor set
   # all 5 freqs  in x
   xtbV =   tbV[ic, ir, , ]
   xtbH =   tbH[ic, ir, , ]
   xtbV2 = tbV2[ic, ir, , ]
   xtbH2 = tbH2[ic, ir, , ]
   xmpdi = mpdi[ic, ir, , ]
   xmpdi2 = mpdi2[ic, ir, , ]

   # one freq in y
   yem = ems[ic, ir, ich, ]

   # verification predictor set 
   vxtbV =   vtbV[ic, ir, , ]
   vxtbH =   vtbH[ic, ir, , ]
   vxtbV2 = vtbV2[ic, ir, , ]
   vxtbH2 = vtbH2[ic, ir, , ]
   vxmpdi = vmpdi[ic, ir, , ]
   vxmpdi2 = vmpdi2[ic, ir, , ]

   # one freq in y
   vyem = vems[ic, ir, ich, ]
   
   # method 1: single channel MPDI: 10G  and its square (2-predictor)
   x=t( rbind(xmpdi[1, ], xmpdi2[1, ]) ) 
   model = lm(yem ~ x)  
   vx=t( rbind(vxmpdi[1, ], vxmpdi2[1, ]) ) 
   errs1[ic, ir, ich]=sqrt( mean( (vyem-predict(model, list(x=vx)) )^2, na.rm=T ) )

   # method 2: five-channel MPDI: 10~89G, linear terms only (5-predictor) 
   x=t( rbind(xmpdi) )
   model = lm(yem ~ x)
   vx=t( rbind(vxmpdi) )
   errs2[ic, ir, ich]=sqrt( mean( (vyem-predict(model, list(x=vx)) )^2, na.rm=T ) )

   # method 3: 10-channel Tbs: 10~89G, linear terms only (10-predictor) 
   x=t(rbind(xtbV, xtbH) )
   model = lm(yem ~ x)
   vx=t(rbind(vxtbV, vxtbH) )
   errs3[ic, ir, ich]=sqrt( mean( (vyem-predict(model, list(x=vx)) )^2, na.rm=T ) )

   # method 4: 10-channel Tb and 5-channel MPDI, linear terms only (15-predictor) 
   x=t(rbind(xtbV, xtbH, xmpdi) )
   model = lm(yem ~ x)
   vx=t(rbind(vxtbV, vxtbH, vxmpdi) )
   errs4[ic, ir, ich]=sqrt( mean( (vyem-predict(model, list(x=vx)) )^2, na.rm=T ) )

   # method 5: 10-channel Tb, 10-channel Tb^2, and 5-channel MPDI (25-predictor) 
   x=t(rbind(xtbV, xtbH, xtbV2, xtbH2, xmpdi) )
   model = lm(yem ~ x)
   vx=t( rbind(vxtbV, vxtbH, vxtbV2, vxtbH2, vxmpdi) ) 
   errs5[ic, ir, ich]=sqrt( mean( (vyem-predict(model, list(x=vx)) )^2, na.rm=T ) )

  }
 }
}

plot_err(lon, lat, errs1*100, "M1")
plot_err(lon, lat, errs2*100, "M2")
plot_err(lon, lat, errs3*100, "M3")
plot_err(lon, lat, errs4*100, "M4")
plot_err(lon, lat, errs5*100, "M5")

# print the numbers

cat("\n  Table X: Comparison of Five Prediction Methods\n") 
cat("====================================================================\n") 
cat("     |  10V     10H     19V     19H     37V     37H     89V     89H\n")
cat("--------------------------------------------------------------------\n") 
errs=errs1
cat(paste("M1:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs2
cat(paste("M2:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs3
cat(paste("M3:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs4
cat(paste("M4:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs5
cat(paste("M5:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 

cat("====================================================================\n") 
cat("method 1: single channel MPDI: 10G  and its square (2-predictor)\n")
cat("method 2: five-channel MPDI: 10~89G, linear terms only (5-predictor) \n")
cat("method 3: 10-channel Tbs: 10~89G, linear terms only (10-predictor) \n")
cat("method 4: 10-channel Tb and 5-channel MPDI, linear terms only (15-predictor) \n")
cat("method 5: 10-channel Tb, 10-channel Tb^2, and 5-channel MPDI (25-predictor) \n")


