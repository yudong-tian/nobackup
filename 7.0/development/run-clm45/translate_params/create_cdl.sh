# create an empty LDT file, with values filled in later  
domainf=/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/share/domains/domain.lnd.fv0.9x1.25_gx1v6.090309.nc
surfdataf=/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/lnd/clm2/surfdata_map/surfdata_0.9x1.25_simyr1850_c130415.nc
ncdump -h $domainf | egrep '(int |float |double ).*\(.*\)' > domain.vars.txt
#ncdump -h $surfdataf | egrep '(int |float |double ).*\(.*\)' > surfdata.vars.txt
ncdump -h $surfdataf | egrep '[[:space:]](int |float |double ).*;$' > surfdata.vars.txt

cdlf="ldt_clm45.cdl" 

cat > $cdlf <<EOF
netcdf lis_input.clm45 {
dimensions:
        east_west = 288 ;
        north_south = 192 ;
        east_west_b = 292 ;
        north_south_b = 196 ;
        east_west_GDAS_T126 = 384 ;
        north_south_GDAS_T126 = 190 ;
        east_west_GDAS_T170 = 512 ;
        north_south_GDAS_T170 = 256 ;
        east_west_GDAS_T254 = 768 ;
        north_south_GDAS_T254 = 384 ;
        east_west_GDAS_T382 = 1152 ;
        north_south_GDAS_T382 = 576 ;
        east_west_GDAS_T574 = 1760 ;
        north_south_GDAS_T574 = 880 ;
        east_west_GDAS_T1534 = 3072 ;
        north_south_GDAS_T1534 = 1536 ;
        month = 12 ;
        time = 12 ;
        sfctypes = 36 ;
        soilfracbins = 1 ;
        elevbins = 1 ;

        ncorners = 4; 

        lsmlon = 288 ;
        lsmlat = 192 ;
	nglcec = 10 ;
	nglcecp1 = 11 ;
	numurbl = 3 ;
	nlevurb = 5 ;
	numrad = 2 ;
	nchar = 256 ;
	nlevsoi = 10 ;
	lsmpft = 17 ;

variables:
EOF

cat domain.vars.txt | while read var; do

  newvar=`echo $var | sed 's/ni/east_west/g' |sed 's/nj/north_south/g' |sed 's/nv/ncorners/g'|sed 's/mask/LANDMASK/g'`
  varname=`echo $newvar | sed 's/[,();]/ /g' | awk '{print \$2}'`

  cat >> $cdlf <<EOF1
  $newvar
                $varname:standard_name = "$varname" ;
                $varname:units = "" ;
                $varname:scale_factor = 1.f ;
                $varname:add_offset = 0.f ;
                $varname:missing_value = -9999.f ;
                $varname:vmin = 0.f ;
                $varname:vmax = 0.f ;
                $varname:num_bins = 1 ;
EOF1
done

cat >> $cdlf <<EOFa
        float SURFACETYPE(sfctypes, north_south, east_west) ;
                SURFACETYPE:standard_name = "Surface type" ;
                SURFACETYPE:units = "-" ;
                SURFACETYPE:scale_factor = 1.f ;
                SURFACETYPE:add_offset = 0.f ;
                SURFACETYPE:missing_value = -9999.f ;
                SURFACETYPE:vmin = 0.f ;
                SURFACETYPE:vmax = 0.f ;
                SURFACETYPE:num_bins = 36 ;
        float LANDCOVER(sfctypes, north_south, east_west) ;
                LANDCOVER:standard_name = "AVHRR UMD landcover map" ;
                LANDCOVER:units = "" ;
                LANDCOVER:scale_factor = 1.f ;
                LANDCOVER:add_offset = 0.f ;
                LANDCOVER:missing_value = -9999.f ;
                LANDCOVER:vmin = 0.f ;
                LANDCOVER:vmax = 0.f ;
                LANDCOVER:num_bins = 36 ;
EOFa

cat surfdata.vars.txt | while read var; do

  newvar=`echo $var | sed 's/lsmlon/east_west/g' |sed 's/lsmlat/north_south/g' |sed 's/time/month/g' ` 
  #newvar=$var 
  varname=`echo $newvar | sed 's/[,();]/ /g' | awk '{print \$2}'`

  cat >> $cdlf <<EOF2
  $newvar
                $varname:standard_name = "$varname" ;
                $varname:units = "" ;
                $varname:scale_factor = 1.f ;
                $varname:add_offset = 0.f ;
                $varname:missing_value = -9999.f ;
                $varname:vmin = 0.f ;
                $varname:vmax = 0.f ;
                $varname:num_bins = 1 ;
EOF2
done

cat >> $cdlf <<EOF3
        float lat(north_south, east_west) ;
                lat:standard_name = "latitude" ;
                lat:units = "degrees_north" ;
                lat:scale_factor = 1.f ;
                lat:add_offset = 0.f ;
                lat:missing_value = -9999.f ;
                lat:vmin = 0.f ;
                lat:vmax = 0.f ;
        float lon(north_south, east_west) ;
                lon:standard_name = "longitude" ;
                lon:units = "degrees_east" ;
                lon:scale_factor = 1.f ;
                lon:add_offset = 0.f ;
                lon:missing_value = -9999.f ;
                lon:vmin = 0.f ;
                lon:vmax = 0.f ;
        float lat_b(north_south_b, east_west_b) ;
                lat_b:standard_name = "latitude_b" ;
                lat_b:units = "degrees_north" ;
                lat_b:scale_factor = 1.f ;
                lat_b:add_offset = 0.f ;
                lat_b:missing_value = -9999.f ;
                lat_b:vmin = 0.f ;
                lat_b:vmax = 0.f ;
        float lon_b(north_south_b, east_west_b) ;
                lon_b:standard_name = "longitude_b" ;
                lon_b:units = "degrees_east" ;
                lon_b:scale_factor = 1.f ;
                lon_b:add_offset = 0.f ;
                lon_b:missing_value = -9999.f ;
                lon_b:vmin = 0.f ;
                lon_b:vmax = 0.f ;

// global attributes:
		:MAP_PROJECTION = "EQUIDISTANT CYLINDRICAL" ;
		:SOUTH_WEST_CORNER_LAT = -89.5290f ;
		:SOUTH_WEST_CORNER_LON = 0.625f ;
		:DX = 1.25f ;
		:DY = 0.942f ;
                :MAP_PROJECTION_GDAS_T126 = "GAUSSIAN" ;
                :NORTH_WEST_CORNER_LAT_GDAS_T126 = 89.277f ;
                :NORTH_WEST_CORNER_LON_GDAS_T126 = 0.f ;
                :SOUTH_EAST_CORNER_LAT_GDAS_T126 = -89.277f ;
                :SOUTH_EAST_CORNER_LON_GDAS_T126 = -0.938f ;
                :DI_GDAS_T126 = 0.938f ;
                :N_GDAS_T126 = 95.f ;
                :MAP_PROJECTION_GDAS_T170 = "GAUSSIAN" ;
                :NORTH_WEST_CORNER_LAT_GDAS_T170 = 89.463f ;
                :NORTH_WEST_CORNER_LON_GDAS_T170 = 0.f ;
                :SOUTH_EAST_CORNER_LAT_GDAS_T170 = -89.463f ;
                :SOUTH_EAST_CORNER_LON_GDAS_T170 = -0.703f ;
                :DI_GDAS_T170 = 0.703f ;
                :N_GDAS_T170 = 128.f ;
                :MAP_PROJECTION_GDAS_T254 = "GAUSSIAN" ;
                :NORTH_WEST_CORNER_LAT_GDAS_T254 = 89.642f ;
                :NORTH_WEST_CORNER_LON_GDAS_T254 = 0.f ;
                :SOUTH_EAST_CORNER_LAT_GDAS_T254 = -89.642f ;
                :SOUTH_EAST_CORNER_LON_GDAS_T254 = -0.469f ;
                :DI_GDAS_T254 = 0.469f ;
                :N_GDAS_T254 = 192.f ;
                :MAP_PROJECTION_GDAS_T382 = "GAUSSIAN" ;
                :NORTH_WEST_CORNER_LAT_GDAS_T382 = 89.761f ;
                :NORTH_WEST_CORNER_LON_GDAS_T382 = 0.f ;
                :SOUTH_EAST_CORNER_LAT_GDAS_T382 = -89.761f ;
                :SOUTH_EAST_CORNER_LON_GDAS_T382 = -0.313f ;
                :DI_GDAS_T382 = 0.313f ;
                :N_GDAS_T382 = 288.f ;
                :MAP_PROJECTION_GDAS_T574 = "GAUSSIAN" ;
                :NORTH_WEST_CORNER_LAT_GDAS_T574 = 89.844f ;
                :NORTH_WEST_CORNER_LON_GDAS_T574 = 0.f ;
                :SOUTH_EAST_CORNER_LAT_GDAS_T574 = -89.844f ;
                :SOUTH_EAST_CORNER_LON_GDAS_T574 = -0.205f ;
                :DI_GDAS_T574 = 0.205f ;
                :N_GDAS_T574 = 440.f ;
                :MAP_PROJECTION_GDAS_T1534 = "GAUSSIAN" ;
                :NORTH_WEST_CORNER_LAT_GDAS_T1534 = 89.91f ;
                :NORTH_WEST_CORNER_LON_GDAS_T1534 = 0.f ;
                :SOUTH_EAST_CORNER_LAT_GDAS_T1534 = -89.91f ;
                :SOUTH_EAST_CORNER_LON_GDAS_T1534 = -0.1171875f ;
                :DI_GDAS_T1534 = 0.1171875f ;
                :N_GDAS_T1534 = 768.f ;
		:INC_WATER_PTS = "false" ;
		:LANDCOVER_SCHEME = "CESM" ;
		:BARESOILCLASS = 1 ;
		:URBANCLASS = 0 ;
		:SNOWCLASS = 0 ;
		:WATERCLASS = 36 ;
		:WETLANDCLASS = 0 ;
		:GLACIERCLASS = 0 ;
		:NUMVEGTYPES = 35 ;
		:LANDMASK_SOURCE = "AVHRR" ;
		:SFCMODELS = "CLM.4.5" ;
		:SOILTEXT_SCHEME = "Soil texture not selected" ;
		:LAISAI_DATA_INTERVAL = "monthly" ;
		:title = "Land Data Toolkit (LDT) parameter-processed output" ;
		:institution = "NASA GSFC Hydrological Sciences Laboratory" ;
		:history = "created on date: 2015-09-14T12:32:09.195" ;
		:references = "Kumar_etal_EMS_2006, Peters-Lidard_etal_ISSE_2007" ;
		:comment = "website: http://lis.gsfc.nasa.gov/" ;
data:

 mxsoil_color = 20 ;

 month = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ;

}
EOF3


ncgen -o ldt_clm45.nc ldt_clm45.cdl

