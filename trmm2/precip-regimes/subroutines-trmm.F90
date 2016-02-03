! Collection of subroutines: 
! read_GEOS5()
! read_3B42()
! read_mask()
! geos5_correct() 

   subroutine read_GEOS5 (lflag, x, nc, nr, it, fd) 
	integer lflag, nc, nr, it
	real x(nc, nr), mask(nc, nr) 
        character*2 fd, cmon, cdy, chr, cmn
        character*4 cyear
        character*80 filen
	integer tout(9), ic, ir

        if (lflag .EQ. 1.0 ) call read_mask(mask, nc, nr) 

	call gmtime(it+90*60, tout)  ! GEOS timestamp is in the middle of 3hr

        write(cyear, '(I4.4)') tout(6) + 1900 
        write(cmon, '(I2.2)') tout(5) + 1 
        write(cdy, '(I2.2)') tout(4) 
        write(chr, '(I2.2)') tout(3) 
        write(cmn, '(I2.2)') tout(2) 

        filen = "/home/yudong/GEOS5-0.25/" //fd // "/" // cyear // & 
               "/" // cyear // cmon &
               // cdy // "/prectot_H00_" //cyear//cmon // cdy &
               // "_" // chr // cmn // "-0.25.1gd4r" 
        !write(*, *) "reading ", filen 
        open(19, file=filen, form="unformatted", access="direct", &
              recl = nc*nr*4)
            read(19, rec=1) x
        close(19)
        !convert rainrate to mm/day  from mm/s
        x = x * 60*60 
        !mask out
        if (lflag .EQ. 1.0) then 
         Do ir=1, nr
          Do ic = 1, nc 
            if (mask(ic, ir) .EQ. 0 ) x(ic, ir) = -9999.0 
          End Do
         End Do
        end if

      	return
	end  
                  

   subroutine read_3B42 (lflag, x, nc, nr, it) 
        integer lflag, nc, nr, it
        real x(nc, nr), mask(nc, nr)
        character*2 fd, cmon, cdy, chr, cmn
        character*4 cyear
        character*80 filen
        integer tout(9), ic, ir

        if (lflag .EQ. 1.0 ) call read_mask(mask, nc, nr)

        call gmtime(it, tout)  ! 3B42 timestamp

        write(cyear, '(I4.4)') tout(6) + 1900
        write(cmon, '(I2.2)') tout(5) + 1
        write(cdy, '(I2.2)') tout(4)
        write(chr, '(I2.2)') tout(3)
        write(cmn, '(I2.2)') tout(2)

        filen = "/trmm1/TRMM/DATA/3B42-V6/" // cyear &
             // cmon // "/3B42V6." //cyear// cmon &
             // cdy // chr 
        !write(*, *) "reading ", filen
        open(19, file=filen, form="unformatted", access="direct", &
          recl = nc*nr*4)
          read(19, rec=1) x
        close(19)
        ! 3B42 unit is mm/h       
        if (lflag .EQ. 1.0) then
          Do ir = 1, nr
           do ic = 1, nc
            if (mask(ic, ir) .EQ. 0 ) x(ic, ir) = -9999.0
           end Do
          End Do 
        end if

        return
        end


    subroutine read_mask(mask, nc, nr)
     integer nc, nr, ic, ir
     real mask(nc, nr) 

     open(21, file="/home/yudong/proj-disk/input/UMD-25KM/UMD_mask0.25.1gd4r", &
           form="unformatted", access="direct", recl=nc*4)
           Do ir = 41, 440
            read(21, rec=ir) mask(:, ir-40)
           End do
     close(21)
     return 
     End 


! Correct input GEOS5 data.
! Input:
!     rain(nc, nr) -- raw GEOS5 forecasts from 50S - 50N,
!                      interpolated to 0.25x0.25-deg
!                      unit: mm/h
!     nc, nr -- dimension of rain(), same as 3B42-V6, i.e., 1440x400
!     fd --  Forecast day, string, "D1", "D2", ..., or "D5"

! Output:
!     rain(nc, nr) -- corrected GEOS5 forecasts, mm/h

! Required files:
! ncdf-D1-LUT.dat
! ncdf-D2-LUT.dat
! ncdf-D3-LUT.dat
! ncdf-D4-LUT.dat
! ncdf-D5-LUT.dat


     subroutine geos5_correct(rain, nc, nr, fd)
      integer, parameter :: maxbin = 5000  !LUT size
      integer nc, nr
      real rain(nc, nr)
      character*2 fd
      real xlut(maxbin), ylut(maxbin)
      integer ix, iy,  j
      real x1, x2, y1, y2, x, y

      open(19, file="ncdf-"//fd//"-LUT.dat", form="formatted", status="old")
        Do i=1, maxbin
          read(19, *) xlut(i), ylut(i)
        End Do
      close(19)

      Do iy=1, nr
        Do ix=1, nc
         !find the match
         x = rain (ix, iy) ! already converted to mm/h
         if(x .GE.0 .and. x .LT. xlut(1) ) then
           y = 0.0
         else
          Do j=1, maxbin-1
           x1=xlut(j)
           x2=xlut(j+1)
           If(  x .GE. x1 .and. x .LE. x2 ) then

            y1=ylut(j)
            y2=ylut(j+1)
            !linear interp for small values
             y = y1 + (y2 - y1)*(x - x1)/ (x2 - x1)
             exit
            End if
           End Do
          end if
          rain(ix, iy) = y
          !write(*, *) x, "-->", y

        End Do
      End Do

      return
      End

