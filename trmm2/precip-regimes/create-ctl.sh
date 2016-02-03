

for dy in D1 D2 D3 D4 D5 DA; do 

cat > GEOS5-$dy-modified.ctl <<EOF
DSET  ^GEOS5-$dy-modified.1gd4r
options big_endian template
TITLE  ARCTAS $dy forecast @ 1DD
undef -9999
xdef   360 linear  -179.5 1.0
ydef   100 linear  -49.5 1.0
ZDEF    1   LINEAR           1    1
TDEF    330 LINEAR  12Z1apr2008  24hr
VARS     1
prectot 1  99  daily total (mm/day)
ENDVARS

EOF

done 

