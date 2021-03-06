module perf_mod

! YDT empty replacement of the original ~/CESM/cesm1_2_2_dev/models/glc/cism/glimmer-cism/libgptl/perf_mod.F90
!----------------------------------------------------------------------- 
! 
! Purpose: This module is responsible for controlling the performance
!          timer logic.
! 
! Author:  P. Worley, January 2007
!
! $Id$
! 
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!- Uses ----------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!- module boilerplate --------------------------------------------------
!-----------------------------------------------------------------------
   implicit none
   private                   ! Make the default access private
   save

!-----------------------------------------------------------------------
! Public interfaces ----------------------------------------------------
!-----------------------------------------------------------------------
!   public t_initf
   public t_startf
   public t_stopf

!=======================================================================
contains
!=======================================================================

!
!
!========================================================================
!
   subroutine t_startf(event, handle)
!----------------------------------------------------------------------- 
! Purpose: Start an event timer
! Author: P. Worley 
!-----------------------------------------------------------------------
!---------------------------Input arguments-----------------------------
  character(len=*), intent(in) :: event
  integer, optional :: handle
!

   return
   end subroutine t_startf
!
!========================================================================
!
   subroutine t_stopf(event, handle)
!----------------------------------------------------------------------- 
! Purpose: Stop an event timer
! Author: P. Worley 
!-----------------------------------------------------------------------
!---------------------------Input arguments-----------------------------
!
  character(len=*), intent(in) :: event
  integer, optional :: handle
   return
   end subroutine t_stopf
!

!===============================================================================

end module perf_mod
