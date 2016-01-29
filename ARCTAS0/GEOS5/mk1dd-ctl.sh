
for lead in 1 2 3 4 5; do 

 cat > prectot-1dd-D$lead.ctl <<EOF
DSET  ^D$lead/%y4/%m2/prectot-1DD.1gd4r
options big_endian template
TITLE  ARCTAS & GEOS5 00Z forecast @ 1DD. lead time: $lead day(s) 
undef 1e+15
XDEF   360 LINEAR        -179.5   1.0
YDEF   180 LINEAR         -89.5   1.0
ZDEF    1   LINEAR           1    1
TDEF    448 LINEAR  12Z1apr2008  24hr
VARS     1
prectot 1  99  daily total (mm/day) 
ENDVARS

EOF

done

