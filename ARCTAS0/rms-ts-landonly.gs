
* Display RMS as function of time 

'reinit'

stime='12Z1apr2008 12Z31jul2008'

maxsat=6
satdata.1 ='D1/prectot-1dd.ctl' 
satdata.2 ='D2/prectot-1dd.ctl' 
satdata.3 ='D3/prectot-1dd.ctl' 
satdata.4 ='D4/prectot-1dd.ctl' 
satdata.5 ='D5/prectot-1dd.ctl' 
satdata.6 ='gpcp.ctl' 
satname.1='1-day Fcst' 
satname.2='2-day Fcst' 
satname.3='3-day Fcst' 
satname.4='4-day Fcst' 
satname.5='5-day Fcst' 
satname.6='GPCP' 

'open 3B42V6-1dd.ctl'

sat=1
while (sat < maxsat)
  'open 'satdata.sat
  'open GPCC/monthly-clim-gpcc.ctl'

  'set time 'stime

  'set lat -49.5 49.5'
  'set lon -179.5 179.5' 
  'define rain1=maskout(rain.1, rain.3(t=1))'
  'define rain2=maskout(prectot.2, rain.3(t=1))'

  'define mse=(rain2-rain1)*(rain2-rain1)' 

     'set grads off'
*     'set ylint 5'
     'set xlopts 1 0.5 0.15'
     'set ylopts 1 0.5 0.15'

  'set x 1'
  'set y 1'
  'define rmse=aave(mse, lon=-179.5, lon=179.5, lat=-49.5, lat=49.5)' 
*  'set vrange 2 12' 
*  'd sqrt(rmse)' 

  'set t 3 120'
*  'set cmark 0'
*    'set gxout fwrite'
*    'set fwrite -be -st ts-5rains-'satname.sat'-'regname.reg'.1gd4r'
*  'd tloop(ave(sqrt(rmse), t-5, t+5))'
  'set vrange 2 12' 
  'd tloop(ave(sqrt(rmse), t-2, t+2))'

  'close 3'
  'close 2'
sat = sat + 1
endwhile

*** do GPCP separately *********
  'open 'satdata.sat
  'open GPCC/monthly-clim-gpcc.ctl'
  'set time 'stime

  'set lat -49.5 49.5'
  'set lon -179.5 179.5' 
  'define rain1=maskout(rain.1, rain.3(t=1))'
  'define rain2=maskout(rain.2, rain.3(t=1))'
  'define mse=(rain2-rain1)*(rain2-rain1)'

  'set x 1'
  'set y 1'
  'define rmse=aave(mse, lon=-179.5, lon=179.5, lat=-49.5, lat=49.5)'
*  'set vrange 2 12'
*  'd sqrt(rmse)'

  'set t 3 120'
*  'set cmark 0'
*    'set gxout fwrite'
*    'set fwrite -be -st ts-5rains-'satname.sat'-'regname.reg'.1gd4r'
*  'd tloop(ave(sqrt(rmse), t-5, t+5))'
  'set vrange 2 12'
  'd tloop(ave(sqrt(rmse), t-2, t+2))'



  'draw ylab RMSE (mm/day)'
  'draw title ARCTAS 1-5 days forecast (re 3B42) over land' 
  'cbar_l -x 2.3 -y 7.4 -n 6 -t "1-day" "2-day" "3-day" "4-day" "5-day" "GPCP"'

*'printim no-smth-rms-ts.gif gif white x1200 y1000'
'printim rms-ts-landonly.gif gif white x1200 y1000'


