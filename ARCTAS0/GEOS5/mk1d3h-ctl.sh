
for lead in 1 2 3 4 5; do 

 cat > prectot-1d3h-D$lead.ctl <<EOF
DSET  ^D$lead/%y4/%y4%m2%d2/prectot_H00_%y4%m2%d2_%h2%n2_1x1.1gd4r
options big_endian template
TITLE  GEOS5 00Z forecast lead time: $lead day(s) 
undef 1e+15
XDEF   360 LINEAR        -179.5   1.0
YDEF   180 LINEAR         -89.5   1.0
ZDEF    1   LINEAR           1    1
TDEF    2248 LINEAR  1:30Z16aug2008  3hr
VARS     1
prectot 1  99  3hr avg precip (mm/s)
ENDVARS

EOF

done
