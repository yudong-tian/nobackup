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
! !ROUTINE: clm45_writerst
! \label{clm45_writerst}
! 
! !INTERFACE:
subroutine clm45_writerst(n)
! !USES:

  implicit none
! !ARGUMENTS: 
  integer, intent(in)  :: n
!
! !DESCRIPTION:
!
!  Forcing-only (clm45) option for calling the write restart routines. 
! 
!  The arguments are: 
!  \begin{description}
!  \item[n]
!   index of the nest
!  \end{description}
!EOP

end subroutine clm45_writerst
