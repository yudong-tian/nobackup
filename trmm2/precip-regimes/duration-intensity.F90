! This program computes the duration and average intensity of 
! rainfall events from 3B42 data, to see of there are identifiable 
!  regimes in the phase space of (duration, intensity).  
! 
! Currently hard-coded to read 0.25-degree 3B42-V6 or GEOS5 forecasts
! Different data sources were read with different subroutines
!
! usage: duration-intensity <syr> <smon> <sdy> <eyr> <emon> <edy> \
!                   <dataset> 
! <syr> <smon> <sdy> <eyr> <emon> <edy>: start/end year/month/day
! dataset: GEOS5 3B42 

! Ouput: ascii file with 5 space delimited fields: 
! event-duration event-intensity i j event-time(epoch sec)
! -----------------------------------------------------------


        Program duration 

        implicit NONE
        integer, parameter :: dt0 = 3*60*60   ! 3hr time resolution (sec) 
        !  input data dim
        integer, parameter :: nc=1440, nr=400 
        integer, parameter :: mc=360, mr=100  ! 4 x 4 grids 
        !  event separtion threshold ( number of 3-hr steps)  
        integer, parameter :: gap0=4

        ! time management 
        integer st(9), et(9), fmktime
        ! start/end date
        integer :: syr, smon, sdy, eyr, emon, edy

        ! raw data, duration, intensity, and gaps between event
        ! duration and gaps are in unit of 3hr step
        real x(nc, nr), rr(mc, mr), rr0
        integer dur(mc, mr), gap(mc, mr), dur0 
        real rain
        integer ibin, mbin 
        integer i, j, is, js, im, jm, it,  iargc, Ns
        character*80 ctmp, dset
        
        i =  iargc()
        If (i.LT.7) Then
         call getarg(0, ctmp) !** get program name
         Write(*, *) "Usage: ", trim(ctmp), " syr smon sdy eyr emon edy \"
         write(*, *)" <dataset> "
         Write(*, *) "dataset: GEOS5 3B42"
         Stop
        End If

       call getarg(1, ctmp)
       read(ctmp, *) syr 
       call getarg(2, ctmp)
       read(ctmp, *) smon
       call getarg(3, ctmp)
       read(ctmp, *) sdy 
       call getarg(4, ctmp)
       read(ctmp, *) eyr
       call getarg(5, ctmp)
       read(ctmp, *) emon
       call getarg(6, ctmp)
       read(ctmp, *) edy

       call getarg(7, ctmp)
       dset = trim(ctmp)

        st=0
        et=0
        st(6) = syr-1900
        st(5) = smon -1
        st(4) = sdy
        st(3) = 0

        et(6) = eyr-1900
        et(5) = emon -1
        et(4) = edy
        et(3) = 21

       dur = 0 
       rr = 0
       gap = 0

     do it =fmktime(st), fmktime(et), dt0 
         if (trim(dset) .eq. 'GEOS5') then 
           call read_GEOS5 (0, x, nc, nr, it, 'D2') 
         else if (trim(dset) .eq. '3B42') then 
           call read_3B42 (0, x, nc, nr, it) 
         else
           write(*, *) "Unrecognized data set. stop"
           stop
         end if 
         
      Do j = 1, mr
        Do  i = 1, mc

         dur0 = 0  
         rr0 = 0
         Do js=1, 4
           Do is=1, 4
            im=(i-1)*4 + is
            jm=(j-1)*4 + js
            rain = x(im, jm) 
            If( rain .GE. 0.1 ) then  ! rain threshold
                dur0 = dur0 + 1
                rr0 = rr0 + rain
            End if
           End Do ! is
         End Do ! js

         if (dur0 .GT. 0) then  ! subdomain has rain
            dur(i, j) = dur(i, j) + 1
            rr(i, j) = rr(i, j) + rr0 
            gap(i, j) = 0
         Else  ! no rain 
            if (dur(i, j) .GT. 0) then ! previous rainy event
              gap(i, j) = gap(i, j) + 1
            end if 
            
            !enough event separation? 
            if (gap(i, j) .GE. gap0 ) then
               write(*, *) dur(i, j), rr(i, j)/dur(i, j), i, j, it 
               ! reset everything 
               dur(i, j) = 0 
               rr(i, j)=0
               gap(i, j)=0 
            end if 
         End if

        End Do  ! i 
       End Do  ! j 

      End Do  !it

     End

