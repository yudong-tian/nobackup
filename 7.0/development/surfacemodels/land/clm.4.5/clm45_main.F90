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
  use clmtype
  use clm45_lsmMod
  use clm45_driver 
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
  use clm_atmlnd   ,   only : clm_a2l

  implicit none
! !ARGUMENTS: 
  integer, intent(in) :: n 



!  real(r8), intent(in) :: obliqr  !Earth's obliquity in radians
!  real(r8), intent(in) :: lambm0  !Mean longitude of perihelion at the vernal equinox (radians)
!  real(r8), intent(in) :: mvelpp  !Earth's moving vernal equinox long. of perihelion + pi (radians)
! -----------------------------------------------------------------

! ---------------------- local variables --------------------------
  integer  :: j,k,t,m           !loop/array indices
  integer  :: p, c, l, g           ! indices for pft, column, landunit, and grid 
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
  
  REAL     :: n2u   ! function to convert NaNs to undefs and real*8 to real*4 
  Logical  :: isudef  ! function to check if a number is undef 
  REAL     :: soilmr, tmp1, tmp2, tmp3, udef
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
    real(r8) :: tmp_num       !temperary
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

! ----------------------------------------------------------------------
! Write global average diagnostics to standard output
! ----------------------------------------------------------------------

  udef = LIS_rc%udef 
  write(LIS_logunit, *)'LIS_rc%npatch(n,LIS_rc%lsm_index)=', LIS_rc%npatch(n,LIS_rc%lsm_index)
  do k=1, LIS_rc%npatch(n,LIS_rc%lsm_index)

      c=pft%column(k) 
      l=pft%landunit(k) 
      g=pft%gridcell(k) 

     write(LIS_logunit, '(A, I6, A, F10.4, A, F10.4)') 'k=', k, ' pef%fsa(k)=', pef%fsa(k), &
           ' pef%eflx_lwrad_net(k)=', pef%eflx_lwrad_net(k)

     call LIS_diagnoseSurfaceOutputVar(n, k, LIS_MOC_SWNET, &
          value= n2u( pef%fsa(k) ), &
          vlevel=1,unit="W/m2", direction="DN",&
          surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_LWNET, &
          value= -1.0 * n2u( pef%eflx_lwrad_net(k) ), vlevel=1,unit="W/m2", &
          direction="DN",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QLE,&
          value= n2u( pef%eflx_lh_tot(k) ) , vlevel=1,unit="W/m2",&
          direction="UP",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QH,&
          value=n2u( pef%eflx_sh_tot(k) ), &
          vlevel=1,unit="W/m2", direction="UP",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QG,&
          value=n2u( pef%eflx_soil_grnd(k) ), vlevel=1,unit="W/m2",&
          direction="DN",surface_type=LIS_rc%lsm_index)

!Bowen Ratio - sensible/latent
     tmp1 = n2u(pef%eflx_lh_tot(k))
     tmp2 = n2u(pef%eflx_sh_tot(k))
     if( isudef(tmp1) .or. isudef(tmp2) .or. tmp1 .eq. 0 ) then
        call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_BR,value=0.0,vlevel=1,unit="-",&
             direction="-",surface_type=LIS_rc%lsm_index)
     else
        call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_BR,&
          value= tmp2 / tmp1, &
          vlevel=1,unit="-", direction="-",surface_type=LIS_rc%lsm_index)
     endif

!Evaporative Fraction
     
     tmp1 = n2u( pef%eflx_lh_tot(k) )
     tmp2 = n2u( pef%eflx_sh_tot(k) )
     if( isudef(tmp1) .or. isudef(tmp2) .or. tmp1+tmp2 .eq. 0 ) then 
        call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_EF,value=1.0,vlevel=1,unit="-",&
             direction="-",surface_type=LIS_rc%lsm_index)
     else
        call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_EF, &
          value= tmp1 / (tmp1 + tmp2) , vlevel=1,unit="-", &
          direction="-",surface_type=LIS_rc%lsm_index)
     endif

     call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_TOTALPRECIP,&
          value=n2u( clm_a2l%forc_snow(g) + clm_a2l%forc_rain(g) ), &
          vlevel=1,unit="kg/m2s", direction="DN",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_TOTALPRECIP,&
          value=n2u( (clm_a2l%forc_snow(g) + clm_a2l%forc_rain(g))*LIS_rc%nts(n) ) ,&
          vlevel=1,unit="kg/m2", direction="DN") ! EMK
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_SNOWF,&
          value=n2u( clm_a2l%forc_snow(g) ), &
          vlevel=1,unit="kg/m2s", direction="DN",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_RAINF,&
          value=n2u( clm_a2l%forc_rain(g) ), &
          vlevel=1,unit="kg/m2s", direction="DN",surface_type=LIS_rc%lsm_index)

     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_EVAP,&
          value=n2u( pwf%qflx_evap_tot(k) ), &
          vlevel=1,unit="kg/m2s", direction="UP",surface_type=LIS_rc%lsm_index)
     
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QS,&
          value=n2u( cwf%qflx_surf(c) + cwf%qflx_qrgwl(c) ), &
          vlevel=1,unit="kg/m2s", direction="OUT",surface_type=LIS_rc%lsm_index)

     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QS,&
          value=n2u( (cwf%qflx_surf(c)+ cwf%qflx_qrgwl(c) ) * LIS_rc%nts(n) ), &
          vlevel=1,unit="kg/m2", direction="OUT",surface_type=LIS_rc%lsm_index) ! EMK
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QSB,&
          value=n2u( cwf%qflx_drain(c) ) ,&
          vlevel=1,unit="kg/m2s", direction="OUT",surface_type=LIS_rc%lsm_index)

     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QSB,&
          value=n2u( cwf%qflx_drain(c) * LIS_rc%nts(n) ), &
          vlevel=1,unit="kg/m2", direction="OUT",surface_type=LIS_rc%lsm_index) ! EMK
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QSM,&
          value=n2u( cwf%qflx_snomelt(c) ), &
          vlevel=1,unit="kg/m2s", direction="S2L",surface_type=LIS_rc%lsm_index)

     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_QSM,&
          value=n2u( cwf%qflx_snomelt(c) * LIS_rc%nts(n) ), &
          vlevel=1,unit="kg/m2", direction="S2L",surface_type=LIS_rc%lsm_index) ! EMK

     soimtc = 0.0
     soilmr = 0.0
     roottemp = 0.0

!gradual fix ---vvvv ---------------
#if 0
!YDT needs to check soil layers in CLM45 

     do m=1,nlevsoi
        vol_ice(m) = min(cps%watsat(c, m),&
             cws%h2osoi_ice(m)/&
             (XXX%dz(m)*denice))
        eff_porosity(m) = XXX%watsat(m)-vol_ice(m)
        vol_liq(m) = min(eff_porosity(m), &
             XXX%h2osoi_liq(m)/&
             (XXX%dz(m)*denh2o))
        call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_SOILMOIST,&
             value=(vol_liq(m)+vol_ice(m)),&
             vlevel = m, unit="m3/m3", direction="-",surface_type=LIS_rc%lsm_index)
        soimtc = soimtc + vol_liq(m)+vol_ice(m)
        if(XXX%lakpoi) then
           tempvar = XXX%t_lake(m)
        else
           tempvar = XXX%t_soisno(m)
        endif
        call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_SOILTEMP, value=tempvar, &
             vlevel=m, unit="K",direction="-",surface_type=LIS_rc%lsm_index)

        if(m.le.8) then
           if(m.eq.8) then
              soilmr = soilmr +(vol_liq(m)+vol_ice(m))*0.17110706
           else
              soilmr=soilmr + (vol_liq(m)+vol_ice(m))*XXX%dz(m)
           endif
        endif

        if(m.le.8) then
           if(m.eq.8) then
              roottemp = roottemp + tempvar*0.17110706
           else
              roottemp=roottemp + tempvar*XXX%dz(m)
           endif
        endif
     enddo

! rootmoist = 1m soil moisture
     call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_ROOTMOIST, &
          value=1000.0*soilmr, vlevel=1,&
          unit="kg/m2",direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_ROOTTEMP, &
          value=roottemp, vlevel=1,&
          unit="K",direction="-",surface_type=LIS_rc%lsm_index)

     call LIS_diagnoseSurfaceOutputVar(n,k,LIS_MOC_DELSOILMOIST, value=&
          XXX%soilmtc_prev-soimtc, vlevel=1, unit="kg/m2",&
          direction="INC",surface_type=LIS_rc%lsm_index)

     snowtemp = 0
     if (XXX%itypwat/=istwet)then
        if(XXX%snl < 0)then
           totaldepth=0.
           do i=XXX%snl+1,0    ! Compute total depth of snow layers
              totaldepth=totaldepth+XXX%dz(i)
           enddo

           do i=XXX%snl+1,0    ! Compute snow temperature
              snowtemp=snowtemp+&
                   (XXX%t_soisno(i)*XXX%dz(i))
           enddo
           snowtemp=snowtemp/totaldepth
        endif
        if(snowtemp.eq.0)snowtemp=LIS_rc%udef
     endif
     call LIS_diagnoseSurfaceOutputVar(n, k, LIS_MOC_SNOWT, &
          value=snowtemp, vlevel=1,unit="K",direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k, LIS_MOC_VEGT, &
          value=XXX%t_veg,&
          vlevel=1,unit="K",direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k, LIS_MOC_BARESOILT, &
          value=XXX%t_grnd,&
          vlevel=1,unit="K",direction="-",surface_type=LIS_rc%lsm_index)

!----------------------------------------------------------------------
! AvgSurfT is the average surface temperature which depends on
! the snow temperature, bare soil temperature and canopy temperature
!----------------------------------------------------------------------
     snowt=0.
     if (XXX%itypwat/=istwet)then
        if(XXX%snl < 0)then
           snowt =XXX%t_soisno(XXX%snl+1)
        endif
     endif
     if(snowt ==0.)snowt =LIS_rc%udef
     if(snowt.ne.LIS_rc%udef)then
        asurf=XXX%frac_sno*snowt+ &
             XXX%frac_veg_nosno*XXX%t_veg+  &
             (1-(XXX%frac_sno+XXX%frac_veg_nosno))* &
             XXX%t_grnd
     else
        asurf=XXX%frac_veg_nosno*XXX%t_veg+ &
             (1-XXX%frac_veg_nosno)*XXX%t_grnd
     endif

     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_AVGSURFT, &
          value=asurf,vlevel=1,unit="K",direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_RADT, &
          value=XXX%t_rad,&
          vlevel=1,unit="K",direction="-",surface_type=LIS_rc%lsm_index)

#endif
!gradual fix ---^^^^^^--------------


!gradual fix ---vvvv ---------------
#if 0
!now surfalb is not defined. got direct and diffusive alb: albd albi
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_ALBEDO, &
          value=XXX%surfalb,&
          vlevel=1,unit="-",direction="-",surface_type=LIS_rc%lsm_index)
#endif
!gradual fix ---^^^^^^--------------

     tmp1 = n2u( pwf%qflx_evap_veg(k) ) 
     tmp2 = n2u( pwf%qflx_tran_veg(k) ) 
     if ( isudef(tmp1) .or. isudef(tmp2) ) then   
       call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_ECANOP, value= LIS_rc%udef, &
          vlevel=1,unit="kg/m2s",direction="UP",surface_type=LIS_rc%lsm_index)
     else 
       call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_ECANOP, value=&
          n2u( pwf%qflx_evap_veg(k) -  pwf%qflx_tran_veg(k) ),&
          vlevel=1,unit="kg/m2s",direction="UP",surface_type=LIS_rc%lsm_index)
     end if 

     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_TVEG, value=&
          n2u( pwf%qflx_tran_veg(k)),vlevel=1,unit="kg/m2s",&
          direction="UP",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_ESOIL, value=&
          n2u( pwf%qflx_evap_veg(k)),vlevel=1,unit="kg/m2s",&
          direction="UP",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_SUBSNOW, value=&
          n2u( pwf%qflx_sub_snow(k)), vlevel=1,unit="kg/m2s",&
          direction="-",surface_type=LIS_rc%lsm_index)

!gradual fix ---vvvv ---------------
#if 0
!YDT: in cws, cwf, or cps
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_SWE, &
          value=XXX%h2osno,&
          vlevel=1,unit="kg/m2",direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_SWE, &
          value=XXX%h2osno/LIS_CONST_RHOFW,&
          vlevel=1,unit="m",direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_SNOWDEPTH, &
          value=XXX%snowdp,&
          vlevel=1,unit="m",direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_SNOWCOVER, vlevel=1,unit="-",&
          value=XXX%frac_sno,direction="-",&
          surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_CANOPINT, value=&
          XXX%h2ocan,vlevel=1,unit="kg/m2",&
          direction="-",surface_type=LIS_rc%lsm_index)
     call LIS_diagnoseSurfaceOutputVar(n, k,LIS_MOC_DELSWE,vlevel=1,unit="kg/m2",value=&
          XXX%h2osno-XXX%h2osno_prev,&
          direction="INC",surface_type=LIS_rc%lsm_index)
#endif
!gradual fix ---^^^^^^--------------
!YDT acond undefined now 
!     call LIS_diagnoseSurfaceOutputVar(n, k, LIS_MOC_ACOND,&
!          value=XXX%acond,&
!          vlevel=1,unit="m/s",direction="-",surface_type=LIS_rc%lsm_index)
  enddo

#if 0
!YDT: in cws, cwf, cps, or on grid (forcing) 
  clm45_struc(n)%count=clm45_struc(n)%count+1
  soilmtc = 0.0

  if(LIS_rc%tscount(n)==0 .or. LIS_rc%tscount(n) ==1 &
       .or. LIS_rc%rstflag(n).eq.1 ) then
     do m=1,nlevsoi
        do t=1, LIS_rc%npatch(n,LIS_rc%lsm_index)
           soilm(t,m)=clm45_struc(n)%clm(t)%h2osoi_liq(m)+&
                clm45_struc(n)%clm(t)%h2osoi_ice(m)
        enddo
     enddo
     do m=1,nlevsoi
        do t=1, LIS_rc%npatch(n,LIS_rc%lsm_index)
           soilmtc(t)=soilmtc(t)+soilm(t,m)
        enddo
     enddo
     do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
        clm45_struc(n)%clm(t)%soilmtc_prev = soilmtc(t)
        clm45_struc(n)%clm(t)%h2osno_prev = clm45_struc(n)%clm(t)%h2osno
     enddo
  endif

  clm45_struc(n)%forc_count = 0
#endif
!gradual fix ---^^^^^^--------------

endif   ! --- end of  if(alarmCheck) then 
return
end subroutine clm45_main


! Fucntion to change NaNs into LIS undefs. Convert double to single precision  
 function n2u(x) result (ans) 

  use LIS_coreMod, only : LIS_rc

   real*8, intent(in) :: x 
   real :: ans 
   
   if (isnan(x) ) then 
        ans=LIS_rc%udef 
   else 
        ans = real(x)   
   end if 

   return 
 end function n2u


! Fucntion to check if a real is undef 
 function isudef(x) result (ans)

  use LIS_coreMod, only : LIS_rc

   real, intent(in) :: x
   logical :: ans

   ans=.false.
   if ( x .eq. LIS_rc%udef ) ans=.true. 

   return
 end function isudef 


