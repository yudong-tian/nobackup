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
! !ROUTINE: clm45_main
! \label{clm45_main}
! 
! !INTERFACE: 
subroutine clm45_main (n)
! !USES: 
  use shr_kind_mod, only: r8 => shr_kind_r8

!  use LIS_precisionMod
  use LIS_timeMgrMod, only : LIS_isAlarmRinging
  use clm45_lsmMod
  use clm_varpar, only : nlevsoi
  use clm_varcon   
  use LIS_coreMod, only : LIS_rc
  use LIS_timeMgrMod  , only : LIS_get_curr_calday, LIS_get_nstep
#if (defined RTM)
  use RtmMod        , only : Rtmriverflux
#endif

#if (defined COUP_CSM)
  use clm_csmMod    , only : csm_dosndrcv, csm_recv, csm_send, csm_flxave, &
                             dorecv, dosend, csmstop_now
#endif

  use LIS_histDataMod
  use LIS_constantsMod,  only : LIS_CONST_RHOFW

  implicit none
! !ARGUMENTS: 
  integer, intent(in) :: n 



!  real(r8), intent(in) :: obliqr  !Earth's obliquity in radians
!  real(r8), intent(in) :: lambm0  !Mean longitude of perihelion at the vernal equinox (radians)
!  real(r8), intent(in) :: mvelpp  !Earth's moving vernal equinox long. of perihelion + pi (radians)
! -----------------------------------------------------------------

! ---------------------- local variables --------------------------
  integer  :: j,k,t,m           !loop/array indices
!  real :: tvegb(144,76)
  integer  :: dtime               !timestep size [seconds]
  integer  :: nstep               !timestep index
!  real(r8) :: buf1d(numpatch)     !temporary buffer 
!  real(r8) :: tsxyav              !average ts for diagnostic output
#if (defined SPMD)
!  integer :: numrecvv(0:npes-1)   !vector of items to be received  
!  integer :: displsv(0:npes-1)    !displacement vector
!  integer :: numsend              !number of items to be sent
#endif
  real(r8) :: cgrnd
  real(r8) :: cgrndl
  real(r8) :: cgrnds
  real(r8) :: tg
  real(r8) :: emg
  real(r8) :: htvp
  real(r8) :: dlrad
  real(r8) :: ulrad
  real(r8) :: tssbef(-5:10)
  
  REAL     :: soilmr
  REAL     :: roottemp
  real :: vol_ice(1:nlevsoi)
  real :: vol_liq(1:nlevsoi)
  real :: eff_porosity(1:nlevsoi)
  real :: snowtemp, snowt, totaldepth, asurf, tempvar, soimtc
  integer :: i
  logical             :: alarmCheck
  integer             :: iret
  character*3   :: fnest
  

!YDT local variables for clm_drv()

    logical :: doalb                      ! .true. ==> do albedo calculation on this time step
    logical :: rstwr_sync                 ! .true. ==> write restart file before returning
    logical :: rstwr                      ! .true. ==> write restart file before returning
    logical :: nlend_sync                 ! Flag signaling last time-step
    logical :: nlend                      ! .true. ==> last time-step
    logical :: dosend                     ! true => send data back to driver
    real(r8):: nextsw_cday                ! calday from clock of next radiation computation
    real(r8):: caldayp1                   ! clm calday plus dtime offset
    integer :: shrlogunit,shrloglev       ! old values for share log unit and log level
    logical,save :: first_call = .true.         ! first call work
    logical  :: glcrun_alarm          ! if true, sno data is averaged and sent to glc this step
    logical  :: update_glc2sno_fields ! if true, update glacier_mec fields
    real(r8) :: calday                ! calendar day for nstep
    real(r8) :: declin                ! solar declination angle in radians for nstep
    real(r8) :: declinp1              ! solar declination angle in radians for nstep+1
    real(r8) :: eccf                  ! earth orbit eccentricity factor
    real(r8) :: recip                 ! reciprical
    character(len=32)            :: rdate       ! date char string for restart file names

! -----------------------------------------------------------------
!  call t_startf('clm45_main')

  write(fnest,'(i3.3)') n
  alarmCheck = LIS_isAlarmRinging(LIS_rc, "CLM45 model alarm "//trim(fnest))
  if(alarmCheck) then 

     nstep = LIS_get_nstep(LIS_rc,n)
     
     dtime = LIS_rc%nts(n)
     caldayp1 = LIS_get_curr_calday(LIS_rc,offset=dtime)  

   call clm45_drv(n, doalb, nextsw_cday, declinp1, declin, rstwr, nlend, rdate)

  endif
 return
end subroutine clm45_main

