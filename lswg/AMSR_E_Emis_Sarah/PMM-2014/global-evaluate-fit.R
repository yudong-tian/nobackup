
# using the first 2588 time-steps of data for model fitting, 
# the rest 2589-3287 time-steps for validation

fit_plot <- function (xm, ym, title) {
   out = nls( ym ~ a + b * xm + c * xm * xm, start=list(a=1, b=1, c=0), 
              control=nls.control(minFactor=1/4096, warnOnly = T))
   plot(xm, ym, xlab="mpdi", ylab="emis")
   points(xm[!is.na(xm)], predict(out), col="blue")

   abc=coef(out)
   #err=sd(residuals(out))
   err=sqrt( mean( (residuals(out))^2 ) ) 
   #ca=paste(c("a=", "b=", "c=", "err="), round(c(abc, err), 2), sep="")
   cerr=paste("err*100=", round(err, 3)*100, sep="")
   #text(0.00, c(1, 0.99, 0.98, 0.97), labels=ca, col="blue", adj=0)
   # text(0.00, c(0.7, 0.71, 0.72, 0.73), labels=ca, col="blue", adj=0)
   title(main=title) 
   mtext(cerr, 3)  # show errors

}


nc=1440
nr=720 
nt=365
nch=12
lon0=0.125
lat0=-89.875

d1=as.Date("2006-07-01")

#read in location file: name, lat, lon

loc=read.table("/home/ytian/lswg/AMSR_E_TB/all-locations.txt", 
                col.names=c("name", "lat", "lon") )
attach(loc)
# convert lon to 0-360
lon[lon < 0]=lon[ lon < 0 ] + 360

nloc=length(name)

emis=array(-9999.0, dim=c(nch, nloc, nt), dimnames=c("channel", "loc", "time"))

# read daily data for 1 year (365 days) 
for (iday in 1:nt) {
    dfile=format(d1+iday-1, "Emiss.AMSR-E.%Y%m%dD.v03.dat") 
    print(paste("reading ", dfile)) 

   to.read=file(paste("/home/ytian/lswg/CREST/Jul06-Jun07-bin/", dfile, sep=""), "rb")
   x =readBin(to.read, numeric(), n=nc*nr*nch, size=4, endian="big")
   close(to.read)
   x[x==-9999.0]=NA
   data=array(x, c(nc, nr, nch), dimnames=c("x", "y", "var"))

   # channel lineup 
	#em7V         1  99  **  1 amsre 6.925  Ghz H-pol
	#em7H         1  99  **  1 amsre 6.925  Ghz V-pol
	#em11V         1  99  **  1 amsre 10.65  Ghz H-pol
	#em11H         1  99  **  1 amsre 10.65  Ghz V-pol
	#em19V         1  99  **  1 amsre 18.7  Ghz H-pol
	#em19H         1  99  **  1 amsre 18.7  Ghz V-pol
	#em24V         1  99  **  1 amsre 23.8 Ghz H-pol
	#em24H         1  99  **  1 amsre 23.8 Ghz H-pol
	#em37V         1  99  **  1 amsre 36.5   Ghz H-pol
	#em37H         1  99  **  1 amsre 36.5   Ghz V-pol
	#em89V         1  99  **  1 amsre 89.0   Ghz H-pol
	#em89H         1  99  **  1 amsre 89.0   Ghz V-pol

   for(iloc in 1:nloc) { 
     ic = as.integer( (lon[iloc] - lon0)/0.25 ) + 1
     ir = as.integer( (lat[iloc] - lat0)/0.25 ) + 1
     emis[ , iloc, iday] = data[ic, ir, ] 
     #print(paste(name[iloc], " lat=", lat[iloc], " lon=", lon[iloc], " ic=", ic, " ir=", ir)) 
   } # iloc
}  # iday 
     
em7V         =emis[1, , ] 
em7H         =emis[2, , ] 
em11V        =emis[3, , ] 
em11H        =emis[4, , ]
em19V        =emis[5, , ]
em19H        =emis[6, , ]
em24V        =emis[7, , ]
em24H        =emis[8, , ]
em37V        =emis[9, , ]
em37H        =emis[10, , ]
em89V        =emis[11, , ]
em89H        =emis[12, , ]

mpdi= (em11V - em11H)/(em11V + em11H)

for(iloc in 1:nloc) { 

  par(mfrow=c(2,3))
  par(bg="white")
  
 # assuming missing values are common for channels
  xm = na.omit( mpdi[iloc, ] ) 

  if (length(xm) > 30 ) { 
    ym = na.omit(em19V[iloc, ] ) 
    fit_plot(xm, ym, paste("19V at", name[iloc]) ) 
    ym = na.omit(em37V[iloc, ] ) 
    fit_plot(xm, ym, paste("37V at", name[iloc]) ) 
    ym = na.omit(em89V[iloc, ] ) 
    fit_plot(xm, ym, paste("89V at", name[iloc]) ) 

    ym = na.omit(em19H[iloc, ] ) 
    fit_plot(xm, ym, paste("19H at", name[iloc]) )
    ym = na.omit(em37H[iloc, ] ) 
    fit_plot(xm, ym, paste("37H at", name[iloc]) )
    ym = na.omit(em89H[iloc, ] ) 
    fit_plot(xm, ym, paste("89H at", name[iloc]) )

    dev.copy(postscript, paste("locations/", name[iloc], "-eval-fits.ps", sep=""), horizontal=T)
    dev.off()
  } #end if 

}

