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
  use LIS_logMod,         only : LIS_verify
  use clm45_lsmMod

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

  integer            :: t,tid,status, g
  type(ESMF_Field)   :: tmpField,q2Field,uField,vField,swdField,lwdField
  type(ESMF_Field)   :: psurfField,pcpField,cpcpField,snowfField
  real,pointer       :: tmp(:),q2(:),uwind(:),vwind(:),snowf(:)
  real,pointer       :: swd(:),lwd(:),psurf(:),pcp(:),cpcp(:)


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

#if 0 

! 1-d grid space, begg - endg 
  do t=1,LIS_rc%npatch(n,LIS_rc%lsm_index)
     g=LIS_surface(n,LIS_rc%lsm_index)%tile(t)%tile_id  ! to be changed to grid id 

     if(LIS_FORC_Tair%selectOpt.eq.1) then
       clm45_struc(n)%clm_a2l%forc_t(g)=tmp(g)
     endif
     if(LIS_FORC_Qair%selectOpt.eq.1) then
       clm45_struc(n)%clm_a2l%forc_q(g)=q2(g)
     endif
     if(LIS_FORC_SWdown%selectOpt.eq.1) then
       clm45_struc(n)%clm_a2l%forc_solar(g)=swd(g)
     endif
     if(LIS_FORC_LWdown%selectOpt.eq.1) then
       clm45_struc(n)%clm_a2l%forc_lwrad(g)=lwd(g)
     endif
     if(LIS_FORC_Wind_E%selectOpt.eq.1) then
       clm45_struc(n)%clm_a2l%forc_u(g)=uwind(g)
     endif
     if(LIS_FORC_Wind_N%selectOpt.eq.1) then
       clm45_struc(n)%clm_a2l%forc_v(g)=vwind(g)
     endif
     if(LIS_FORC_Psurf%selectOpt.eq.1) then
       clm45_struc(n)%clm_a2l%forc_pbot(g)=psurf(g)
     endif
     if(pcp(g).ne.LIS_rc%udef) then
        clm45_struc(n)%clm_a2l%rainf(g)=pcp(g)
     else
        clm45_struc(n)%clm_a2l%rainf(g)=0.0
     endif
!     if(LIS_FORC_CRainf%selectOpt.eq.1) then 
!        if(cpcp(g).ne.LIS_rc%udef) then 
!           clm45_struc(n)%clm_a2l%rainf_c=cpcp(g)
!        else
!           clm45_struc(n)%clm_a2l%rainf_c=0.0
!        endif
!     endif
!     if(LIS_FORC_Snowf%selectOpt.eq.1) then 
!        if(snowf(g).ne.LIS_rc%udef) then
!           clm45_struc(n)%clm_a2l%snowf=snowf(g)
!        else
!           clm45_struc(n)%clm_a2l%snowf=0.0
!        endif
!     endif
  enddo

#endif


end subroutine clm45_f2t
