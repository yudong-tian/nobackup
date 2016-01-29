
# This script reprojects 3hrly GESO5 2/3x1/2 grid to GPCP 1DD grid 


## H00 forecast
fc=00
ovar=prectot   # output var: total precip 

#t0="Sep 1 2008"   # starting time: 0Z
#t0="Oct 27 2008"   # starting time: 0Z
#t1="Oct 31 2008"   # end time: 0Z
t0="Nov 3 2008"   # starting time: 0Z
t1="Apr 1 2009"   # end time: 0Z

sec0=`date -u -d "$t0" +%s`
sec1=`date -u -d "$t1" +%s`
let days=(sec1-sec0)/86400

for day in `seq 0 $days`; do
  t1=`date -u -d "$t0 $day day"`
  cyr=`date -u -d "$t1" +%Y` 
  cmn=`date -u -d "$t1" +%m` 
  cdy=`date -u -d "$t1" +%d` 

  # 00-24h forecast: first forecast, 1:30 hr (90 min) ahead, 3h (180min) apart
  # 8 steps forward
  for lead in 1 2 3 4 5; do   # lead time (day)
    ns=0
    let fm=(lead-1)*24*60+90

  while [ $ns -lt 8 ]; do 
    # forecast date and time
    fyr=`date -u -d "$t1 $fm min" +%Y`
    fdate=`date -u -d "$t1 $fm min" +%Y%m%d`
    ftime=`date -u -d "$t1 $fm min" +%H%M`
    fdir="D$lead/$fyr/$fdate"
    echo ${ovar}_H00_${fdate}_${ftime}
    ../geos5-to-1dd $fdir/${ovar}_H00_${fdate}_${ftime}.1gd4r $fdir/${ovar}_H00_${fdate}_${ftime}_1x1.1gd4r

    let fm=fm+180
    let ns=ns+1 
  done  #while 

 done  # for lead
done 

