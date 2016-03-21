
ga-> open prectot.ctl
Scanning description file:  prectot.ctl
Data file prectot.1gd4r is open as file 1
LON set to 0 360
LAT set to -90 90
LEV set to 1 1
Time values set: 2008:8:16:1 2008:8:16:1
ga-> set gxout shaded
ga-> d prectot
Contouring: 0 to 0.0022 interval 0.0002
ga-> sdfopen /archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast/Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf
Scanning self-describing file:  /archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast/Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf
Found 21 data variables in SDF file.
Found displayable variable slp with 0 levels in SDF file.
Found displayable variable phis with 0 levels in SDF file.
Found displayable variable ps with 0 levels in SDF file.
Found displayable variable cape with 0 levels in SDF file.
Found displayable variable cldtot with 0 levels in SDF file.
Found displayable variable cldhgh with 0 levels in SDF file.
Found displayable variable cldmid with 0 levels in SDF file.
Found displayable variable cldlow with 0 levels in SDF file.
Found displayable variable pblh with 0 levels in SDF file.
Found displayable variable disph with 0 levels in SDF file.
Found displayable variable prectot with 0 levels in SDF file.
Found displayable variable preccon with 0 levels in SDF file.
Found displayable variable preclsc with 0 levels in SDF file.
Found displayable variable precanv with 0 levels in SDF file.
Found displayable variable precsno with 0 levels in SDF file.
Found displayable variable tqv with 0 levels in SDF file.
Found displayable variable troppv with 0 levels in SDF file.
Found displayable variable tropp with 0 levels in SDF file.
Found displayable variable troppb with 0 levels in SDF file.
Found displayable variable tropt with 0 levels in SDF file.
Found displayable variable tropq with 0 levels in SDF file.
Data file /archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast/Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf is open as file 2
ga-> q file 2
File 2 :
  Descriptor: /archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast/Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf
  Binary: /archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast/Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf
  Type = Gridded
  Xsize = 540  Ysize = 361  Zsize = 1  Tsize = 1
  Number of Variables = 21
    slp 0 -999 sea_level_pressure
    phis 0 -999 surface geopotential height
    ps 0 -999 surface_pressure
    cape 0 -999 cape_for_surface_parcel
    cldtot 0 -999 total_cloud_area_fraction
    cldhgh 0 -999 cloud_area_fraction_for_high_clouds
    cldmid 0 -999 cloud_area_fraction_for_middle_clouds
    cldlow 0 -999 cloud_area_fraction_for_low_clouds
    pblh 0 -999 Planetary boundary layer height
    disph 0 -999 zero_plane_displacement_height
    prectot 0 -999 total_precipitation
    preccon 0 -999 Surface Conv. rain flux needed by land
    preclsc 0 -999 nonanvil_large_scale_precipitation
    precanv 0 -999 anvil_precipitation
    precsno 0 -999 snowfall
    tqv 0 -999 total_precipitable_water
    troppv 0 -999 tropopause_pressure_based_on_EPV_estimate
    tropp 0 -999 tropopause_pressure_based_on_thermal_estimate
    troppb 0 -999 tropopause_pressure_based_on_blended_estimate
    tropt 0 -999 tropopause_temperature_using_blended_TROPP_estimate
    tropq 0 -999 tropopause_specific_humidity_using_blended_TROPP_estimate
ga-> q dims
Default file number is: 1
X is varying   Lon = 0 to 360   X = 271 to 811
Y is varying   Lat = -90 to 90   Y = 1 to 361
Z is fixed     Lev = 1  Z = 1
T is fixed     Time = 01:30Z16AUG2008  T = 1
ga-> set lon -180 180
LON set to -180 180
ga-> c
ga-> d prectot
Contouring: 0 to 0.0022 interval 0.0002
ga-> q dims
Default file number is: 1
X is varying   Lon = -180 to 180   X = 1 to 541
Y is varying   Lat = -90 to 90   Y = 1 to 361
Z is fixed     Lev = 1  Z = 1
T is fixed     Time = 01:30Z16AUG2008  T = 1
ga-> d prectot.2(t=1)
Contouring: 0 to 0.0022 interval 0.0002
ga-> d prectot
Contouring: 0 to 0.0022 interval 0.0002
ga-> d prectot.2(t=1)
Contouring: 0 to 0.0022 interval 0.0002
ga-> d prectot-prectot.2(t=1)
Constant field.  Value = 0

