dset ^GPCP-1DD/gpcp_1dd_p1d_%y4%m2%d2.1gd4r
title GPCP V2 1DD
options big_endian template yrev
undef -99999.
xdef   360 linear  0.5  1.0
ydef   180 linear  -89.5 1.0
zdef   1 linear  1 1
tdef 3000 linear 12Z1apr2005 1dy
vars 1
rain   1 99 daily precip  (mm/day)
ENDVARS

