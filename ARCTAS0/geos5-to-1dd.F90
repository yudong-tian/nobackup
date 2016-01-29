
! This program reprojects GEOS5 2/3 x 1/2 grid to 1 x 1 GPCP 1DD grid. 

! usage: geos5-to-1dd inputfile outputfile 

      Program aggr2
      implicit NONE

      integer, parameter :: ic=540, ir=361   !GEOS5 grid 
      integer, parameter :: nc=360, nr=180   !GPCP 1dd
      integer i, j, i1, i2, j1, j2, j3 
      character*120 ctmp, ifile, ofile 
      real :: x(ic, ir), out(nc, nr)  
      real :: wx1, wx2, wy1, wy2, wy3 

      i =  iargc()
      If (i.NE.2) Then
        call getarg(0, ctmp) !** get program name
        Write(*, *) "Usage: ", trim(ctmp), " inputfile outputfile" 
        Stop
      End If

      call getarg(1, ifile) 
      call getarg(2, ofile) 

      open(19, file=ifile, form="unformatted", &
            access="direct", recl=ic*ir*4, status="old")
       read(19, rec=1) x 
      close(19)

      wy1=0.5
      wy2=1.0
      wy3=0.5

      Do j=1, nr
        Do i=1, nc
          if ( mod (i, 2) .eq. 0 ) then 
            wx1 = 1.0
            wx2 = 0.5
          else 
            wx1 = 0.5 
            wx2 = 1.0
          end if 

         i1 = int(i * 1.5 )
         i2 = i1 + 1 
         j1 = j*2-1
         j2 = j*2
         j3 = j*2+1
      
        ! write(*, *) "i,  j,  i1,  i2,  j1,  j2,  j3" 
        ! write(*, '(1x, 4I5, 2F6.2)')i,  j,  i1,  i2,  wx1, wx2 

	 out(i, j) = wx1*wy1*x(i1, j1) + wx2*wy1*x(i2, j1)  &
                   + wx1*wy2*x(i1, j2) + wx2*wy2*x(i2, j2)  & 
  	           + wx1*wy3*x(i1, j3) + wx2*wy3*x(i2, j3) 

        !normalize by total weight
        out(i, j) = out(i, j) / 3.0 

       End Do 
      End Do 


       open(20, file=ofile, form="unformatted", access="direct", &
            recl=nc*nr*4 ) 
          write(20, rec=1) out 
       close(20)

       Stop 
       End 
