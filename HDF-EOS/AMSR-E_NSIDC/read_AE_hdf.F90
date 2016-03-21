! $Id$ 
!
! To compile on discover
!export INC_HDF=/home/ytian/proj-disk/libs/HDF4.2r4/include
!export LIB_HDF=/home/ytian/proj-disk/libs/HDF4.2r4/lib
!cc -c fmktime.c
!ifort -g -u -debug extended -names lowercase -convert big_endian -assume byterecl \
! -I$INC_HDF -L$LIB_HDF -ljpeg -lz -lm -o read_AE_hdf read_AE_hdf.F90 \
! $LIB_HDF/libmfhdf.a $LIB_HDF/libdf.a fmktime.o

        program readae

#include "hdf.f90"
     
        ! for reprojection 
        real, parameter :: lat0=-89.875, lon0=-179.875, res=0.25
        integer, parameter :: nc=1440, nr=720  ! lat/lon grid 
        integer :: mint, iargc   ! slot interval in  minutes 
        integer*4 :: nscan, nray, nfiles, ifile, ierr, ndays, iday, idx1, idx2  
	integer sdfid, sdid, sindex, status, start(2), count(2), stride(2)
        integer sfstart, sfn2index, sfselect, sfginfo, sfrdata, sfendacc, sfend
        integer dimid, rank, dimsizes(32), data_type, num_attrs, i, j, nslot1d
	character*100  ctmp, info, unzfile, filelist, infile, outfile, outdir 
        real*4, allocatable :: lat(:, :), lon(:, :)
        integer*2, allocatable ::  surfrain(:, :) 
!        real*4, allocatable :: slotrain(:, :, :) 
        integer*2, allocatable :: year(:)
        integer :: imid, timestamp0, timestamp1, tsday, tyr, tmon, nslots, islot
        character*1, allocatable :: month(:), day(:), hour(:), minute(:), second(:) 
        integer*4, allocatable :: timestamp(:)   ! in epoch seconds 
        character*4 cseq, cyr
        character*2 cmon, cday 
        ! time management
        integer st(9), st0(9), fmktime
        integer :: ilat, ilon 
        real*4, allocatable :: gridrain(:, :) 
        integer*2, allocatable :: gridcnt(:, :) 
     
        data start /0, 0/ 
        data stride /1, 1/ 
!        data count /nray, nscan/ 

	allocate(gridrain(nc, nr))
	allocate(gridcnt(nc, nr))

        i =  iargc()
        If (i.NE.2) Then
          call getarg(0, ctmp) !** get program name
          Write(*, *) "Usage: ", trim(ctmp), &
            " infile outfile"
          Stop
        End If

       call getarg(1, infile)
       call getarg(2, outfile)

	sdfid = sfstart(infile, DFACC_RDONLY)
        call verify(sdfid, "Failed to open input file")
        write(*, *) "File opened. sdfid=", sdfid
        
        sdid = sfn2index(sdfid, "Rain Rate")
        call verify(sdid, "Failed to get index of Rain Rate") 
        write(*, *) "sdid=", sdid
        
        sindex=sfselect(sdfid, sdid) 
        call verify(sindex, "Failed to select index of Rain Rate") 
        !dimid = sfdimid(sindex, 
        status = sfginfo(sindex, info, rank, dimsizes, data_type, num_attrs) 
        call verify(status, "sfginfo failed")

        write(*, *)"info:", info
	write(*, *)"rank=", rank, "  data_type=", data_type, "  num_attrs=", num_attrs
        if (rank .LT. 1) then 
          write(*, *) "Rank of dimension is wrong. Quit ..."
          stop 
        end if 
        Do i=1, rank
         Write(*, *)"Dimension", i, "=", dimsizes(i)
        End Do 
       
        nray = dimsizes(1)  
        nscan = dimsizes(2)  
        count(1) = nray
        count(2) = nscan 
        allocate (surfrain(nray, nscan))
        allocate (lat(nray, nscan))
        allocate (lon(nray, nscan))


        status=sfrdata(sindex, start, stride, count, surfrain)
        call verify(status, "Reading surfrain failed")

        sdid = sfn2index(sdfid, "Latitude")
        call verify(sdid, "Failed to get index of Latitude")
        write(*, *) "sdid=", sdid
        sindex=sfselect(sdfid, sdid) 
        call verify(sindex, "Failed to select index of Latitude") 
        status=sfrdata(sindex, start, stride, count, lat)
        call verify(status, "Reading Latitude failed")

        sdid = sfn2index(sdfid, "Longitude")
        call verify(sdid, "Failed to get index of Longitude")
        write(*, *) "sdid=", sdid
        sindex=sfselect(sdfid, sdid) 
        call verify(sindex, "Failed to select index of Longitude") 
        status=sfrdata(sindex, start, stride, count, lon)
        call verify(status, "Reading Longitude failed")
! reproj to lat/lon grid
        gridrain=0.0
        gridcnt = 0
            Do i=1, nscan 
             Do j=1, nray 
               if (surfrain(j, i) .GE. 0 ) then
                 ilat = nint ( (lat(j, i) - lat0 )/res ) + 1 
                 ilon = nint ( (lon(j, i) - lon0 )/res ) + 1 
                 gridrain(ilon, ilat) = gridrain(ilon, ilat) + real(surfrain(j, i))*0.1  
                 gridcnt(ilon, ilat) = gridcnt(ilon, ilat) + 1 
               end if
            End Do    ! nray
        End Do  ! nscan

        status=sfendacc(sindex)
        status=sfend(sdfid)

        deallocate(surfrain)
        deallocate(lat)
        deallocate(lon)

!    Do averaging 
           Do ilat =1, nr
             Do ilon=1, nc 
               if(gridcnt(ilon, ilat) .GE. 1 ) then 
                  gridrain(ilon, ilat) = gridrain(ilon, ilat) &
                                              / gridcnt(ilon, ilat) 
               else 
                  gridrain(ilon, ilat) = -99.99 
               end if 
             End do ! ilon 
           End do ! ilat 

         open(20, file=outfile, form="unformatted", access="direct", &
                recl=nc*nr*4) 
            write(20, rec=1) gridrain
         close(20) 
   
        deallocate(gridrain) 
        deallocate(gridcnt) 

        stop
 end        
        
	subroutine verify(status, msg)
          integer :: status
          character (*) :: msg

          if (status.LT.0) then 
           Write(*, *) msg, " status=", status
           Stop 
          end if 
        end 
	
