
! Read GEOS5 forecast data from raw hdfeof file
        program readhd

#include "hdf.f90"
     
        external ::  gdopen, gdattach, gdreadfield, gddetach, gdclose
       
	integer*4, parameter :: nc=540, nr=361, nz=1
	integer*4 gdfid, gdid, status, start(2), count(2), stride(2)
        integer*4 gdopen, gdattach, gdrdfld, gddetach, gdclose
	character*100 geos5f, path
	real*4 prectot(nc, nr)
     
        data start /0, 0/ 
        data stride /1, 1/ 
        data count /nc, nr/ 

!  Archive copy, slow read 
!	path="/archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast/"
!	geos5f="Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf"
	path="/nfs3m/portal/GMAO/gmao_ops/fp/forecast/"
	geos5f="Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf"

	gdfid = gdopen(trim(path)//geos5f, DFACC_RDONLY)
	!gdfid = gdopen(trim(path)//geos5f, DFACC_READ)
        if (gdfid .LT. 0) then 
         write(*, *) "Failed to open input file."
         stop
        end if
        write(*, *) "File opened. gdfid=", gdfid
        
        gdid = gdattach(gdfid, "EOSGRID")
        if (gdid .LT. 0) then 
         write(*, *) "Failed to attach to input file. gdid=", gdid
         stop
        end if
        
        status=gdrdfld(gdid, "prectot", start, stride, count, prectot)
        write(*, *) "Read status: ", status
        open(20, file="prectot.1gd4r", form="unformatted", access="direct", &
                 recl=nc*nr*4)
         write(20, rec=1) prectot
        close(20)

        status=GDdetach(gdid) 
        status=GDclose(gdfid) 
        stop
        end        
        

	
