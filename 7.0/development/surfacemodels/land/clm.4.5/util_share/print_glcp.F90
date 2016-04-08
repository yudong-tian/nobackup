!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.1
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"

subroutine print_glcp(type) 

  ! LIS modules
  use LIS_logMod
  use clmtype
  use clm45_decompMod

! !DESCRIPTION:        
! pring grid, landunit, column, and pft info, for debugging
! Input: type=<'g'|'l'|'c'|'p'> 

!EOP
   implicit none
   integer :: g, l, c, p 
   character*1 :: type

   integer :: begg, endg, begl, endl, begc, endc, begp, endp

      ! check if can get grid and pft range now
      call get_proc_bounds (begg, endg, begl, endl, begc, endc, begp, endp)

      write(LIS_logunit,*) 'Local domain info: '  
      write(LIS_logunit,*) '  begg   endg   begl  endl   begc   endc   begp   endp  '
      write(LIS_logunit,*) '============================================================================' 
      write(LIS_logunit,'(8I7)') begg, endg, begl, endl, begc, endc, begp, endp
      write(LIS_logunit,*) 

  select case (type) 
    case ('g') 
      write(LIS_logunit,*) 'g-type is not supported yet '
    case ('l') 
      write(LIS_logunit,*) 'l-type is not supported yet '
    case ('c') 
      write(LIS_logunit,*) 'c-type is not supported yet '
    case ('p') 
      write(LIS_logunit,*) '  p,    column, wtcol, lunit, wtlunit, gridcell, wtgcell, itype, active'
      write(LIS_logunit,*) '============================================================================' 
       Do p=begp, endp
        write(LIS_logunit,'(2I6, F9.3, I7, F9.3, I7, F9.3, I7, L3)') &
              p, pft%column(p), pft%wtcol(p), pft%landunit(p), pft%wtlunit(p), pft%gridcell(p), &
              pft%wtgcell(p), pft%itype(p), pft%active(p)
       End Do
      write(LIS_logunit,*) '============================================================================' 
      write(LIS_logunit,*) 
                 
    case default 
      write(LIS_logunit,*) 'wrong type '
    end select 

end subroutine print_glcp

