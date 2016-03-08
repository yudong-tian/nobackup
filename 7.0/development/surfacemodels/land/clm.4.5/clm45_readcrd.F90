!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.1
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
!
! !ROUTINE: clm45_readcrd
! \label{clm45_readcrd}
!
! !REVISION HISTORY:
! 21 Jul 2004; Sujay Kumar, Initial Code
! 23 Oct 2007; Kristi Arsenault, Updated for use in LISv5.0
! 23 May 2014: David Mocko, Updated for LIS-7.0
!
! !INTERFACE:    
subroutine clm45_readcrd()

! !USES:
  use ESMF
  use LIS_coreMod,     only : LIS_rc, LIS_config
  use LIS_timeMgrMod, only : LIS_parseTimeString
  use LIS_logMod,      only : LIS_logunit, LIS_verify
  use clm45_lsmMod, only : clm45_struc

!
! !DESCRIPTION:
!
!  This routine reads the options specific to clm45 LSM 
!  option from the LIS configuration file. 
!  
!EOP
  implicit none

  integer :: rc
  integer :: n
  character*10  :: time

  call ESMF_ConfigFindLabel(LIS_config,"CLM45 model timestep:",rc=rc)
  do n=1,LIS_rc%nnest
     call ESMF_ConfigGetAttribute(LIS_config,time,rc=rc)
     call LIS_verify(rc,'CLM45 model timestep: not defined')

     call LIS_parseTimeString(time,clm45_struc(n)%ts)
  enddo

  call ESMF_ConfigFindLabel(LIS_config,"CLM45 pft physiology file:",rc=rc)
  do n=1,LIS_rc%nnest
     call ESMF_ConfigGetAttribute(LIS_config,clm45_struc(n)%clm_vfile,rc=rc)
  enddo

  write(LIS_logunit,*)'Running clm45 LSM Option:'
  do n=1,LIS_rc%nnest
     clm45_struc(n)%clm45open=0
  enddo

end subroutine clm45_readcrd
