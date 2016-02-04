# create an empty LDT file, with values filled in later  
outfile=ldt_clm45.nc
domainf=/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/share/domains/domain.lnd.fv0.9x1.25_gx1v6.090309.nc
surfdataf=/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/lnd/clm2/surfdata_map/surfdata_0.9x1.25_simyr1850_c130415.nc
#ncdump -h $domainf | egrep '(int |float |double ).*\(.*\)' > domain.vars.txt
#ncdump -h $surfdataf | egrep '[[:space:]](int |float |double ).*;$' > surfdata.vars.txt

cat domain.vars.txt | while read var; do

  newvar=`echo $var | sed 's/ni/east_west/g' |sed 's/nj/north_south/g' |sed 's/nv/ncorners/g'|sed 's/mask/LANDMASK/g'`
  varname=`echo $newvar | sed 's/[,();]/ /g' | awk '{print \$2}'`
  cmd=`echo $var | sed 's/[,();]/ /g'`
  
  ovarname=`echo $varname |sed 's/mask/LANDMASK/'`    # mask to LANDMASK

   echo $ovarname $cmd
  ./translate_to_LDT $domainf $outfile $ovarname $cmd 

done

cat surfdata.vars.txt | while read var; do

  newvar=`echo $var | sed 's/lsmlon/east_west/g' |sed 's/lsmlat/north_south/g' |sed 's/time/month/g' ` 
  varname=`echo $newvar | sed 's/[,();]/ /g' | awk '{print \$2}'`
  cmd=`echo $var | sed 's/[,();]/ /g'`

  ovarname=$varname   # no renaming 

   echo $ovarname $cmd
  ./translate_to_LDT $surfdataf $outfile $ovarname $cmd 
done


# fill up lon and lat values 

./translate_to_LDT $domainf $outfile  lon double xc nj ni
./translate_to_LDT $domainf $outfile  lat double yc nj ni

# fill lon_b and lat_b
./translate_to_LDT $domainf $outfile  lon_b double xc nj ni
./translate_to_LDT $domainf $outfile  lat_b double yc nj ni

