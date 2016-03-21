
! Read GEOS5 forecast data from raw hdfeof file
        program readhd

#include "hdf.f90"
     
       
	integer, parameter :: nc=540, nr=361, nz=1
	integer sdfid, sdid, sindex, status, start(2), count(2), stride(2)
        integer sfstart, sfn2index, sfselect, sfrdata, sfendacc, sfend
	character*100 geos5f, path
	real prectot(nc, nr)
     
        data start /0, 0/ 
        data stride /1, 1/ 
        data count /nc, nr/ 

! archive copy, slow read
	!path="/archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/forecast/"
	!geos5f="Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf"

! analysis
!       path="/archive/u/dao_ops/GEOS-5.2.0/GEOSdas-2_1_4/d520_fp/diag/"
!	geos5f="Y2009/M03/d520_fp.tavg2d_met_x.20090328_0130z.hdf"

        path="/nfs3m/portal/GMAO/gmao_ops/fp/forecast/"
        geos5f="Y2009/M03/D30/H00/d520_fp.tavg2d_met_x.20090330_00z+20090402_1930z.hdf"

	sdfid = sfstart(trim(path)//geos5f, DFACC_RDONLY)

        if (sdfid .LT. 0) then 
         write(*, *) "Failed to open input file."
         stop
        end if
        write(*, *) "File opened. sdfid=", sdfid
        
        sdid = sfn2index(sdfid, "PRECTOT")
        if (sdid .LT. 0) then 
         write(*, *) "Failed to attach to input file. sdid=", sdid
         stop
        end if
        write(*, *) "sdid=", sdid
        
        sindex=sfselect(sdfid, sdid) 
        write(*, *) "select status: sindex=", sindex 
        status=sfrdata(sindex, start, stride, count, prectot)
        write(*, *) "Reading status = ", status 

        open(20, file="prectot.1gd4r", form="unformatted", access="direct", &
                 recl=nc*nr*4)
         write(20, rec=1) prectot
        close(20)

        status=sfendacc(sindex) 
        status=sfend(sdfid) 
        stop
        end        
        

	
