
#gdas metforcing interpolation assumes lon in (-180, 180), while
#clm paramters are in (0, 360). This script makes a swapped copy. 

#cp ldt_clm45.cdl ldt_clm45_lonswap.cdl
#manually edit the cdl to make the global attribute line read: 
#                :SOUTH_WEST_CORNER_LON = -180.0f ;

#generate a new nc file with empty data 

ncgen -o ldt_clm45_lonswap.nc ldt_clm45_lonswap.cdl

#copy every variable from orginal ldt to new ldt, after swapping the longitude
ncdump -h ldt_clm45.nc | egrep '(int |float |double ).*\(.*\)' > ldt_vars.txt

cat ldt_vars.txt | while read var; do

  cmd=`echo $var | sed 's/[,();]/ /g'`

  ./do_swap_lon ldt_clm45.nc ldt_clm45_lonswap.nc $cmd

done






