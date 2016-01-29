
*** script to plot zonal-avg 
'reinit'
*** panel layout
cols=1
rows=1
hgap=0.0
vgap=0.1
vh=8.5/rows
vw=11/cols

parea='1.0 10 1.0 7.4'

df.1='../3B42V6-1dd.ctl'
df.2='prectot-1dd-D1.ctl' 
df.3='prectot-1dd-D2.ctl' 
df.4='prectot-1dd-D3.ctl' 
df.5='prectot-1dd-D4.ctl' 
df.6='prectot-1dd-D5.ctl' 


var.1='rain'
var.2='prectot'
var.3='prectot'
var.4='prectot'
var.5='prectot'
var.6='prectot'

*stime='12Z1Jul2008 12Z31Jul2008'
*tname='Jul. 2008' 

stime='12Z1Sep2008 12Z31Oct2008'
tname='Sep-Oct. 2008' 


*** LL-UR lat-lon.row.col sample boxes
minlon.1.1=-179.5
maxlon.1.1=179.5
minlat.1.1=-49.5
maxlat.1.1=49.5
range.1.1='0 11' 
cbar.1.1='-x 7.8 -y 7.1'

col=1
while(col <= cols)
  row = 1
  while (row <= rows)

  vx1=(col-1)*vw+hgap
  vx2=col*vw-hgap
  vy1=(rows-row)*vh+vgap
  vy2=vy1+vh-vgap
  'set vpage 'vx1' 'vx2' 'vy1' 'vy2
  'set grads off'
  'set parea 'parea

  minx=minlon.row.col
  miny=minlat.row.col
  maxx=maxlon.row.col
  maxy=maxlat.row.col

*with 3B42  nf=1
  nf=2
  while (nf <= 6)

  'open 'df.nf 
  say 'opening 'df.nf 
  'set time 'stime 
  'q dims'
  tmp=sublin(result, 5)
  tmin=subwrd(tmp, 11)
  tmax=subwrd(tmp, 13)

  'set xlopts 1 0.5 0.15'
  'set ylopts 1 0.5 0.15'
  'set t 1'
  'set lon 'minx
  'set lat 'miny' 'maxy
  'set vrange 'range.row.col
*  'set ccolor 'clr.nf
  'define avgr=ave(ave('var.nf', lon='minx', lon='maxx'), t='tmin', t='tmax')'
*  'set cmark 0'
*  'set x 1'
*  'set y 1'
*  'set tlsupp year'
  'd avgr'
  'draw ylab Precip (mm/day)'
*  'draw title Zonal-average precip for ARCTAS, 'tname 
  'draw title Zonal-average precip for GEOS5, 'tname 
*  'cbar_l 'cbar.row.col' -n 6 -t "3B42" "1-day" "2-day" "3-day" "4-day" "5-day"' 
  'cbar_l 'cbar.row.col' -n 5 -t "1-day" "2-day" "3-day" "4-day" "5-day"' 
  nf=nf+1
  'close 1'
 endwhile

 row = row+1
endwhile
col = col + 1
endwhile
*'printim zonal-avg.gif gif white x2000 y1600'
*'printim zonal-avg-sm.gif gif white x300 y240'
'printim zonal-avg-sep-oct08.gif gif white x2000 y1600'
'printim zonal-avg-sep-oct08-sm.gif gif white x300 y240'

