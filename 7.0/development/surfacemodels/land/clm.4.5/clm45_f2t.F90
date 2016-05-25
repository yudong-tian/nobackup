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
! !ROUTINE: clm45_f2t
! \label{clm45_f2t}
!
! !REVISION HISTORY: 
! 21 Jul 2004: Sujay Kumar   Initial Specification
! 23 Oct 2007: Kristi Arsenault, Implemented code for LISv5.0
! 
! !INTERFACE:
subroutine clm45_f2t(n)

! !USES:
  use ESMF
  use LIS_coreMod,        only : LIS_rc, LIS_surface
  use LIS_metforcingMod, only : LIS_FORC_State
  use LIS_FORC_AttributesMod 
  use LIS_logMod,         only : LIS_verify, LIS_logunit,  &
                                 LIS_getNextUnitNumber, LIS_releaseUnitNumber
  use LIS_historyMod
  use clmtype
  use clm45_lsmMod
  use clm_atmlnd, only    : clm_a2l
  use clm45_decompMod

  implicit none
! !ARGUMENTS: 
  integer, intent(in)  :: n
!
! !DESCRIPTION:
!
!  Forcing-only option (clm45) for calling the forcing transfer routines.
! 
!  The arguments are: 
!  \begin{description}
!  \item[n]
!   index of the nest
!  \end{description}
!EOP

  integer            :: ftn 
  logical :: isudef
  real :: n2u 
  integer            :: t,tid,status, g, l, c, p, begg, endg, begl, endl, begc, endc, begp, endp
  type(ESMF_Field)   :: tmpField,q2Field,uField,vField,swdField,lwdField
  type(ESMF_Field)   :: psurfField,pcpField,cpcpField,snowfField
  real,pointer       :: tmp(:),q2(:),uwind(:),vwind(:),snowf(:)
  real,pointer       :: swd(:),lwd(:),psurf(:),pcp(:),cpcp(:)

 !tmp tile-space variable
  real, allocatable :: tile_var(:)

  !YDT Note: all the returned variables in ESMF_FieldGet are in tile space 
  if(LIS_FORC_Tair%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_Tair%varname(1)),tmpField,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(tmpField,localDE=0,farrayPtr=tmp,rc=status)
    call LIS_verify(status)
  endif
  
  if(LIS_FORC_Qair%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_Qair%varname(1)),q2Field,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(q2Field,localDE=0,farrayPtr=q2,rc=status)
    call LIS_verify(status)
  endif
  
  if(LIS_FORC_SWdown%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_SWdown%varname(1)),swdField,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(swdField,localDE=0,farrayPtr=swd,rc=status)
    call LIS_verify(status)
  endif

  if(LIS_FORC_LWdown%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_LWdown%varname(1)),lwdField,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(lwdField,localDE=0,farrayPtr=lwd,rc=status)
    call LIS_verify(status)
  endif

  if(LIS_FORC_Wind_E%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_Wind_E%varname(1)),uField,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(uField,localDE=0,farrayPtr=uwind,rc=status)
    call LIS_verify(status)
  endif
  
  if(LIS_FORC_Wind_N%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_Wind_N%varname(1)),vField,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(vField,localDE=0,farrayPtr=vwind,rc=status)
    call LIS_verify(status)
  endif
  
  if(LIS_FORC_Psurf%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_Psurf%varname(1)),psurfField,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(psurfField,localDE=0,farrayPtr=psurf,rc=status)
    call LIS_verify(status)
  endif
  
  if(LIS_FORC_Rainf%selectOpt.eq.1) then
    call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_Rainf%varname(1)),pcpField,&
         rc=status)
    call LIS_verify(status)

    call ESMF_FieldGet(pcpField,localDE=0,farrayPtr=pcp,rc=status)
    call LIS_verify(status)
  endif
  
  if(LIS_FORC_CRainf%selectOpt.eq.1) then 
     call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_CRainf%varname(1)),cpcpField,&
          rc=status)
     call LIS_verify(status)

     call ESMF_FieldGet(cpcpField,localDE=0,farrayPtr=cpcp,rc=status)
     call LIS_verify(status)
  endif

  if(LIS_FORC_Snowf%selectOpt.eq.1) then 
     call ESMF_StateGet(LIS_FORC_State(n),trim(LIS_FORC_Snowf%varname(1)),snowfField,&
          rc=status)
     call LIS_verify(status)

     call ESMF_FieldGet(snowfField,localDE=0,farrayPtr=snowf,rc=status)
     call LIS_verify(status)
  endif

   call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
   write(LIS_logunit, *)'clm45_f2t: begg, endg, begl, endl, begc, endc, begp, endp'
   write(LIS_logunit, '(8I7)') begg, endg, begl, endl, begc, endc, begp, endp
   write(LIS_logunit, *)'-------------------end ----------------------------------'

! Stuff data to  1-d grid space, begg - endg, from tile space
!  do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
!     g=LIS_surface(n,LIS_rc%lsm_index)%tile(t)%tile_id  

  do p= begp, endp 

      c=pft%column(p)
      l=pft%landunit(p)
      g=pft%gridcell(p)

   !YDT: 4/13/16: temp fix: where to get these numbers now?
       pps%displa(p) = 5.  ! displacement height (m)
       pps%z0mv(p) = 12.      !roughness length over vegetation, momentum [m]
       pps%z0m(p) = 10.      !roughness length over vegetation, momentum [m]
       pps%htop(p) = 15.0     ! canopy top (m)

     cps%z0mg(c) = 5.0          !roughness length over ground, momentum

   !YDT: 4/13/16: temp fix: where to get these numbers now?
      clm_a2l%forc_hgt_t(g) = 30.0 
      clm_a2l%forc_hgt_u(g) = 30.0 
      clm_a2l%forc_hgt_q(g) = 30.0 
      clm_a2l%forc_rho(g) = 1.225          ! atmospheric density (kg/m**3)
      clm_a2l%forc_hgt(g) = 6000      ! atm ref height


     if(LIS_FORC_Tair%selectOpt.eq.1) then
       clm_a2l%forc_t(g)=tmp(p)
       clm_a2l%forc_th(g)=tmp(p)  !YDT temp fix 
     else ! what values to take?  
       clm_a2l%forc_t(g)=0.
       clm_a2l%forc_th(g)=0.
     endif
     if(LIS_FORC_Qair%selectOpt.eq.1) then
       clm_a2l%forc_q(g)=q2(p)
     else 
       clm_a2l%forc_q(g)=0. 
     endif
     if(LIS_FORC_SWdown%selectOpt.eq.1) then
       clm_a2l%forc_solar(g)=swd(p)
     else
       clm_a2l%forc_solar(g)=0. 
     endif
     if(LIS_FORC_LWdown%selectOpt.eq.1) then
       clm_a2l%forc_lwrad(g)=lwd(p)
     else 
       clm_a2l%forc_lwrad(g)=0. 
     endif
     if(LIS_FORC_Wind_E%selectOpt.eq.1) then
       clm_a2l%forc_u(g)=uwind(p)
     else 
       clm_a2l%forc_u(g)=0.
     endif
     if(LIS_FORC_Wind_N%selectOpt.eq.1) then
       clm_a2l%forc_v(g)=vwind(p)
     else 
       clm_a2l%forc_v(g)=0. 
     endif
     if(LIS_FORC_Psurf%selectOpt.eq.1) then
       clm_a2l%forc_pbot(g)=psurf(p)
     else 
       clm_a2l%forc_pbot(g)=0.
     endif
     if(pcp(g).ne.LIS_rc%udef) then
        clm_a2l%rainf(g)=pcp(p)
     else
        clm_a2l%rainf(g)=0.0
     endif
!     if(LIS_FORC_CRainf%selectOpt.eq.1) then 
!        if(cpcp(g).ne.LIS_rc%udef) then 
!           clm_a2l%rainf_c=cpcp(p)
!        else
!           clm_a2l%rainf_c=0.0
!        endif
!     endif
!     if(LIS_FORC_Snowf%selectOpt.eq.1) then 
!        if(snowf(g).ne.LIS_rc%udef) then
!           clm_a2l%snowf=snowf(p)
!        else
!           clm_a2l%snowf=0.0
!        endif
!     endif

 enddo  ! p 

  allocate(tile_var( LIS_rc%npatch(n,LIS_rc%lsm_index)) )
  Do t=1, LIS_rc%npatch(n,LIS_rc%lsm_index)
     g=pft%gridcell(t)
     tile_var(t) = n2u( clm_a2l%forc_solar(g) )
  End Do 
  ftn = LIS_getNextUnitNumber()
  open(ftn, file="forc_solar.1gdr4", access="sequential", form="unformatted") 
  call LIS_writevar_bin(ftn, n, tile_var ) 
  call LIS_releaseUnitNumber(ftn)
  deallocate(tile_var) 

end subroutine clm45_f2t
