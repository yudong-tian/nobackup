
# show the coordicates of the LL and UP grid boxes 

nc=288
#nr=192
nr=160

x0=-180
#y0=-90 
y0=-60 

dx=1.25
dy=0.942

echo abc | awk -v nc="$nc" -v nr="$nr" -v x0="$x0" -v y0="$y0" -v dx="$dx" -v dy="$dy" \
    '{print x0+0.5*dx, y0+0.5*dy}' 

echo abc | awk -v nc="$nc" -v nr="$nr" -v x0="$x0" -v y0="$y0" -v dx="$dx" -v dy="$dy" \
    '{print x0+0.5*dx+(nc-1)*dx, y0+0.5*dy+(nr-1)*dy}' 


