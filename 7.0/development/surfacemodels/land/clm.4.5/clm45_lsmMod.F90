!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) v7.1
!
! Copyright (c) 2015 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
module clm45_lsmMod
!BOP
!
! !MODULE: clm45_lsmMod
!
! !DESCRIPTION:
!  Module for 1-D land model driver variable initialization
!  
! \begin{description}
!  \item[count]
!    variable to keep track of the number of timesteps before an output
!  \item[numout]
!    number of output times 
!  \item[outInterval]
!    output writing interval
!  \item[clm45open]
!    variable to keep track of opened files
!  \item[clm45]
!   clm45 LSM specific variables
! \end{description} 
!
! !REVISION HISTORY:
!    Apr 2003; Sujay Kumar, Initial Code
! 23 Oct 2007; Kristi Arsenault, Updated for V5.0
!
! !USES:        

  use clm45_module

  implicit none
  
  PRIVATE
!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  public :: clm45_lsm_ini
  public :: clm45_t2gcr 
!-----------------------------------------------------------------------------
! !PUBLIC TYPES:
!-----------------------------------------------------------------------------
  public :: clm45_struc
!EOP
  type, public :: clm45_type_dec
     integer                    :: clm45open
     integer                    :: numout
     real                       :: ts
     type(clm45dec), allocatable :: clm45(:)
  end type clm45_type_dec
  type(clm45_type_dec), allocatable :: clm45_struc(:)

  SAVE
contains
!BOP
! 
! !ROUTINE: clm45_lsm_ini
! \label{clm45_lsm_ini}
! 
! !INTERFACE:
  subroutine clm45_lsm_ini()
! !USES:
   use ESMF
   use LIS_surfaceModelDataMod, only : LIS_sfmodel_struc
   use LIS_coreMod
   use LIS_timeMgrMod,   only : LIS_clock, LIS_calendar, &
        LIS_update_timestep, LIS_registerAlarm
   use LIS_logMod,       only : LIS_verify, LIS_logunit
! !DESCRIPTION:        
!
!EOP
   implicit none
   integer :: n, i, t, g, gic, gir, gid, git
   integer                 :: yr, mo, da, hr, mn, ss
   integer                 :: status


   write(LIS_logunit,*) 'Inside clm45_lsm_ini() ' 
   write(LIS_logunit,*) '     pe  ntiles   grids  gdeltas goffsets tdeltas toffsets' 
   write(LIS_logunit,*) '===========================================================' 
   do n = 1, LIS_rc%nnest
      do i=0,LIS_npes-1
        write(LIS_logunit,'(7I8)') i, LIS_ntiless(n, i), LIS_ngrids(n, i), LIS_gdeltas(n, i), &   
                                      LIS_goffsets(n, i), LIS_tdeltas(n, i), LIS_toffsets(n, i) 
      end do 
      write(LIS_logunit,*) '-----------------------------------------------------------' 
      write(LIS_logunit,'(A8, 2I8)') 'Total:', LIS_rc%glbntiles(n), LIS_rc%glbngrid(n)
   end do 
   write(LIS_logunit, *) ! empty line

   write(LIS_logunit,*) '     pe  LIS_nss_halo_ind LIS_nss_halo_ind LIS_ews_halo_ind LIS_ewe_halo_ind' 
   write(LIS_logunit,*) '==========================================================='
   do n = 1, LIS_rc%nnest
      do i=0,LIS_npes-1
        write(LIS_logunit,'(5I8)') i, LIS_nss_halo_ind(n, i+1), LIS_nse_halo_ind(n, i+1), & 
                                      LIS_ews_halo_ind(n, i+1), LIS_ewe_halo_ind(n, i+1)
      end do
   end do
   write(LIS_logunit, *) ! empty line

   write(LIS_logunit,*) '     pe  LIS_nss_ind LIS_nss_ind LIS_ews_ind LIS_ewe_ind'
   write(LIS_logunit,*) '==========================================================='
   do n = 1, LIS_rc%nnest
      do i=0,LIS_npes-1
        write(LIS_logunit,'(5I8)') i, LIS_nss_ind(n, i+1), LIS_nse_ind(n, i+1), &
                                      LIS_ews_ind(n, i+1), LIS_ewe_ind(n, i+1)
      end do
   end do
   write(LIS_logunit, *) ! empty line

   write(LIS_logunit,*) 'Local domain info: '  
   write(LIS_logunit,*) '   tile   col  row index  tile_id vegt  lon   lat   gic  gir ntiles stid git' 
   write(LIS_logunit,*) '============================================================================' 
   do n = 1, LIS_rc%nnest
     do t = 1, LIS_rc%ntiles(n) 
         g = LIS_domain(n)%tile(t)%index
           call clm45_t2gcr(n, LIS_localPet, t, gic, gir, git)  ! get global ic, ir from local tile id
           gid = gic+(gir-1)*LIS_rc%gnc(n)
           write(LIS_logunit,'(6I6, 2F8.2, 3I5, 2I7)') t, LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row, &
                                   LIS_domain(n)%tile(t)%index, LIS_domain(n)%tile(t)%tile_id, &
                                   LIS_domain(n)%tile(t)%vegt, &
                                   LIS_domain(n)%grid(g)%lon, LIS_domain(n)%grid(g)%lat, & 
                                   gic, gir,  LIS_domain(n)%ntiles_pergrid(gid), LIS_domain(n)%str_tind(gid), git
     end do 
   end do 
   write(LIS_logunit, *) ! empty line
                                 


   allocate(clm45_struc(LIS_rc%nnest))

   call clm45_readcrd()
   do n = 1, LIS_rc%nnest
      allocate(clm45_struc(n)%clm45(LIS_rc%npatch(n,LIS_rc%lsm_index)))
      clm45_struc(n)%numout = 0

      call LIS_update_timestep(LIS_rc, n, clm45_struc(n)%ts)

      LIS_sfmodel_struc(n)%nsm_layers = 1
      LIS_sfmodel_struc(n)%nst_layers = 1
      allocate(LIS_sfmodel_struc(n)%lyrthk(1))
      LIS_sfmodel_struc(n)%lyrthk(1) = 1
      LIS_sfmodel_struc(n)%ts = clm45_struc(n)%ts
   enddo
  
  end subroutine clm45_lsm_ini

!BOP
!
! !ROUTINE: clm45_t2gcr
! \label{clm45_t2gcr}
!
! !INTERFACE:
  subroutine clm45_t2gcr(n, ipe, it, ic, ir, git)
! !USES:
   use ESMF
   use LIS_surfaceModelDataMod, only : LIS_sfmodel_struc
   use LIS_coreMod
   use LIS_timeMgrMod,   only : LIS_clock, LIS_calendar, &
        LIS_update_timestep, LIS_registerAlarm
   use LIS_logMod,       only : LIS_verify, LIS_logunit
! !DESCRIPTION:
! Subroutine to compute the col and row indices of the global grid, and index of the global tile space, given 
!  the local tile index as input. The tile index ranges from 1 to total tiles for each pe. 
!   
!EOP
   implicit none
   integer, intent(in) :: n, ipe, it
   integer, intent(out) :: ic, ir, git 
   integer :: g, gid, lastg, g0, i, j, ic0, ir0, gid0, lastgrid  ! how many tiles up to last grid

   ic = LIS_domain(n)%tile(it)%col + LIS_ews_ind(n, ipe+1) - 1
   ir = LIS_domain(n)%tile(it)%row + LIS_nss_ind(n, ipe+1) - 1

   lastgrid = 0
   g0=LIS_domain(n)%tile(it)%index   ! current grid index
   lastg = 0 
   Do i=1, it-1
    g=LIS_domain(n)%tile(i)%index 
    if (g .ne. g0 .and. g .ne. lastg ) then ! different grid 
       ic0 = LIS_domain(n)%tile(i)%col + LIS_ews_ind(n, ipe+1) - 1
       ir0 = LIS_domain(n)%tile(i)%row + LIS_nss_ind(n, ipe+1) - 1
       gid0 = ic0+(ir0-1)*LIS_rc%gnc(n)
       lastgrid = lastgrid + LIS_domain(n)%ntiles_pergrid(gid0)
       lastg = g  ! only add once 
    end if 
   End Do 
   gid = ic+(ir-1)*LIS_rc%gnc(n)
   git = (it - lastgrid) + LIS_domain(n)%str_tind(gid) - 1

  end subroutine clm45_t2gcr

end module clm45_lsmMod

