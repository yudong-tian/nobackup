
* Display Bias as function of time 

'reinit'

stime='12Z1jun2006 12Z15sep2006'

maxsat=3
satdata.1 ='ECMWF-1dd/1dd.ctl' 
satdata.2 ='/home/yudong/ARCTAS/gpcp.ctl' 
satdata.3 ='/home/yudong/proj-disk/FORCING/3B42-V6/1dd.ctl' 
satname.1='ECMWF 12-hr Fcst' 
satname.2='GPCP' 
satname.3='3B42' 

sat=1
while (sat <= maxsat)
  'open 'satdata.sat
  'set time 'stime

  'set lat -49.5 49.5'
  'set lon -179.5 179.5' 

     'set grads off'
*     'set ylint 5'
     'set xlint 25'
     'set xlopts 1 0.5 0.15'
     'set ylopts 1 0.5 0.15'

  'set x 1'
  'set y 1'
  'define avg=aave(rain, lon=-179.5, lon=179.5, lat=-49.5, lat=49.5)' 
*  'set vrange 2 12' 
*  'd sqrt(rmse)' 

  'set time 12Z3JUN2006 12Z3SEP2006' 
*  'set cmark 0'
*    'set gxout fwrite'
*    'set fwrite -be -st ts-5rains-'satname.sat'-'regname.reg'.1gd4r'
*  'd tloop(ave(sqrt(rmse), t-5, t+5))'
  'set vrange 0 5' 
  'd tloop(ave(avg, t-2, t+2))'

  'close 1'
sat = sat + 1
endwhile

  'draw ylab Global mean (mm/day)'
  'draw title ECMWF 12-hour, GPCP and 3B42' 
  'cbar_l -x 2.3 -y 7.4 -n 3 -t "ECMWF" "GPCP" "3B42"'

*'printim no-smth-rms-ts.gif gif white x1200 y1000'
'printim 3b42-ecmwf-mean-ts.gif gif white x1200 y1000'


