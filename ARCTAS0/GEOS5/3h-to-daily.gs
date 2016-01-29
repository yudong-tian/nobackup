
*** script to compute daily accumulation (mm/day)
** from
* 3-hourly GEOS5 
* to match GPCP 1DD (0Z-24Z) 
'reinit'

st.1='1:30Z1jan2009'
et.1='1:30Z1feb2009' 

st.2='1:30Z1feb2009' 
et.2='1:30Z1mar2009' 

st.3='1:30Z1mar2009' 
et.3='1:30Z1apr2009' 

mn.1='01'
mn.2='02'
mn.3='03'

leads=5 
df.1='D1' 
df.2='D2' 
df.3='D3' 
df.4='D4' 
df.5='D5' 
var.1='prectot'
ts.1=8
days=1

lead=1
while (lead <= leads) 

'reinit'
'open prectot-1d3h-'df.lead'.ctl' 

nf=3
while (nf <= 3 )

 '!mkdir -p 'df.lead'/2009/'mn.nf
 'set fwrite -be -st 'df.lead'/2009/'mn.nf'/prectot-1DD.1gd4r' 
 say 'set time 'st.nf' 'et.nf
 say 'Working on 'df.lead'/2009/'mn.nf'/prectot-1DD.1gd4r'
 'set time 'st.nf' 'et.nf
 'q dims'
 tmp=sublin(result, 5)
 t0=subwrd(tmp, 11)
 te=subwrd(tmp, 13)
 ts1dy=ts.1*days
 t1=t0+ts1dy-1
 'set gxout fwrite'
 'set time 'st.nf

  while( t1 <= te )
   say 'Doing t0='t0' t1='t1
   'set x 1 360'
   'set y 1 180'
   'define ovar=sum('var.1'*3*3600, t='t0', t='t1')'
   'd ovar' 
   t0=t1+1
   t1=t0+ts1dy-1
  endwhile

  'disable fwrite'
  nf=nf+1
endwhile

lead=lead+1
endwhile

