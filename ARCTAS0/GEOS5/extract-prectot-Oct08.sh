
# This script extracts prectot from ARCTAS GEOS5 HDF file 
# and save as binary. 
# tavg2d - 2-dimensional 3-hour time averaged fields, time-stamped 
# at the center of the averaging period. For example, 04:30z output 
# would be a 3z-6z time average).

dpath=/archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast

## H00 forecast
fc=00
ovar=prectot   # output var: total precip 

t0="Oct 1 2008"   # starting time: 0Z 
t1="Oct 31 2008"   # end time: 0Z

sec0=`date -u -d "$t0" +%s`
sec1=`date -u -d "$t1" +%s`
let days=(sec1-sec0)/86400

for day in `seq 0 $days`; do 
  t1=`date -u -d "$t0 $day day"`  # for new "date" command
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
    if [ ! -d $fdir ]; then
      mkdir -p $fdir
    fi
    gs=/tmp/${fdate}_${ftime}.gs
    echo extracting $fdir/${fdate}_${ftime} from $cyr$cmn${cdy}_${fc}z+${fdate}_${ftime}
    cat > $gs <<EOF
'sdfopen $dpath/Y$cyr/M$cmn/D$cdy/H$fc/d520_fp.tavg2d_met_x.$cyr$cmn${cdy}_${fc}z+${fdate}_${ftime}z.hdf'
'set fwrite -be -st $fdir/${ovar}_H00_${fdate}_${ftime}.1gd4r'
'set gxout fwrite'
'set x 1 540'
'set y 1 361'
'd $ovar '
'disable fwrite'
'quit'
EOF

/usr/local/other/GrADS/1.9b4/bin/gradshdf -blc "run $gs"

    let fm=fm+180
    let ns=ns+1 
  done   # while 

 done   # for lead 
done   # for day

