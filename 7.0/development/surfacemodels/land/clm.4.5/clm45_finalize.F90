!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.1
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
!BOP
!
! !ROUTINE: clm45_finalize
! \label{clm45_finalize}
! 
! !INTERFACE:
subroutine clm45_finalize()
! !USES:
  use LIS_coreMod,  only : LIS_rc
  use clm45_lsmMod, only : clm45_struc

! !ARGUMENTS: 

!
! !DESCRIPTION:
! 
!  This routine cleans up the allocated memory structures in 
!   the clm45 (forcing-only option)
!
!  The arguments are: 
!  \begin{description}
!  \item[n]
!   index of the nest
!  \end{description}
!EOP
  implicit none
  integer :: n

  do n = 1, LIS_rc%nnest
    ! deallocate(clm45_struc(n)%clm45)
  enddo
  deallocate(clm45_struc)

end subroutine clm45_finalize
