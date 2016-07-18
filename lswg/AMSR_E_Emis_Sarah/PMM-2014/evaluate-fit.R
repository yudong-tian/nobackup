
# using the first 2588 time-steps of data for model fitting, 
# the rest 2589-3287 time-steps for validation

fit_plot <- function (xm, ym, vxm, vym, title) { 
   out = nls( ym ~ a + b * xm + c * xm * xm, start=list(a=1, b=1, c=0),
          control=nls.control(minFactor=1/4096, warnOnly = T))
   vx = vxm[!is.na(vxm)]
   vy = vym[!is.na(vym)]
   plot(xm, ym, ylim=c(0.8, 1), xlim=c(0.01, 0.08), main=title, xlab="mpdi", ylab="emis")
   points(vx, predict(out, list(xm=vx) ), col="blue")
   points(vx, vy, col="red")

   abc=coef(out)
   err=sd(vy-predict(out, list(xm=vx)))*100
   ca=paste(c("a=", "b=", "c=", "err%="), round(c(abc, err), 2), sep="")
   text(0.068, c(1, 0.99, 0.98, 0.97), labels=ca, col="blue", adj=0)

}



nc=20
nr=20

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
 
par(mfrow=c(3,2))
par(bg="white")
  

# 11H as function of mpdi at (1, 1) and (10, 10)
ic=1
ir=1
   xm = mpdi[ic, ir, ]
   ym = em11H[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem11H[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "11H at (1, 1)") 

ic=10
ir=10
   xm = mpdi[ic, ir, ]
   ym = em11H[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem11H[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "11H at (10, 10)") 

# 37H as function of mpdi at (1, 1) and (10, 10)
ic=1
ir=1
   xm = mpdi[ic, ir, ]
   ym = em37H[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem37H[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "37H at (1, 1)") 

ic=10
ir=10
   xm = mpdi[ic, ir, ]
   ym = em37H[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem37H[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "37H at (10, 10)") 

# 89H as function of mpdi at (1, 1) and (10, 10)
ic=1
ir=1
   xm = mpdi[ic, ir, ]
   ym = em89H[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem89H[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "89H at (1, 1)") 

ic=10
ir=10
   xm = mpdi[ic, ir, ]
   ym = em89H[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem89H[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "89H at (10, 10)") 

dev.copy(postscript, "plot-eval-fits-H.ps", horizontal=F)
dev.off()

readline(prompt="enter to continue ...") 


par(mfrow=c(3,2))
par(bg="white")


# 11V as function of mpdi at (1, 1) and (10, 10)
ic=1
ir=1
   xm = mpdi[ic, ir, ]
   ym = em11V[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem11V[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "11V at (1, 1)")

ic=10
ir=10
   xm = mpdi[ic, ir, ]
   ym = em11V[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem11V[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "11V at (10, 10)")

# 37H as function of mpdi at (1, 1) and (10, 10)
ic=1
ir=1
   xm = mpdi[ic, ir, ]
   ym = em37V[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem37V[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "37V at (1, 1)")

ic=10
ir=10
   xm = mpdi[ic, ir, ]
   ym = em37V[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem37V[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "37V at (10, 10)")

# 89H as function of mpdi at (1, 1) and (10, 10)
ic=1
ir=1
   xm = mpdi[ic, ir, ]
   ym = em89V[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem89V[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "89V at (1, 1)")

ic=10
ir=10
   xm = mpdi[ic, ir, ]
   ym = em89V[ic, ir, ]
   vxm = vmpdi[ic, ir, ]
   vym = vem89V[ic, ir, ]
   fit_plot(xm, ym, vxm, vym, "89V at (10, 10)")

dev.copy(postscript, "plot-eval-fits-V.ps", horizontal=F)
dev.off()
