#include "LIS_misc.h"

module clm45_read_paramsMod 

   use LIS_coreMod
   use netcdf

   use shr_kind_mod, only : r8 => shr_kind_r8

contains

!BOP
!
! !ROUTINE: read_clm45_param_to_local_g0d_int
! \label{read_clm45_param_to_local_g0d_int} 
! ! Just read one single integer from file 

! !INTERFACE:
  subroutine read_clm45_param_to_local_g0d_int(n, var, varname)
! !USES:
   use LIS_surfaceModelDataMod, only : LIS_sfmodel_struc
   use LIS_coreMod
   use LIS_timeMgrMod,   only : LIS_clock, LIS_calendar, &
        LIS_update_timestep, LIS_registerAlarm
   use LIS_logMod,       only : LIS_verify, LIS_logunit
   use shr_sys_mod , only : shr_sys_flush
! !DESCRIPTION:
! Subroutine to read a global 2D variable from LDT into local variable, 1-D grid space.
!
!EOP
   implicit none
   integer, intent(in) :: n
   integer  :: var
   character(len=*), intent(in) :: varname

    integer                    :: ios
    integer                    :: ncId, nrId
    integer                    :: ncbId, nrbId
    integer                    :: varid, latId,lonId
    integer                    :: latbId,lonbId
    integer                    :: gnc,gnr
    integer                    :: gnc_b, gnr_b
    integer                    :: ftn
    integer                    :: k, gindex
    logical                    :: file_exists

#if (defined USE_NETCDF3 || defined USE_NETCDF4)

       write(LIS_logunit, *) "Reading surface parameter: ", trim(varname) 

          ios = nf90_open(path=LIS_rc%paramfile(n),&
               mode=NF90_NOWRITE,ncid=ftn)
          call LIS_verify(ios,'Error in nf90_open in read_clm45_params:paramfile')

          ios = nf90_inq_varid(ftn, trim(varname), varid)
          call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')

          ios = nf90_get_var(ftn, varid, var)
          call LIS_verify(ios,'Error in nf90_get_var for '// trim(varname) // ' in read_clm45_params')

          ios = nf90_close(ftn)
          call LIS_verify(ios,'Error in nf90_close in read_clm45_params')

#endif

  end subroutine read_clm45_param_to_local_g0d_int

!BOP
!
! !ROUTINE: read_clm45_param_to_local_g1d
! \label{read_clm45_param_to_local_g1d}
!
! !INTERFACE:
  subroutine read_clm45_param_to_local_g1d(n, var, varname, readvar)
! !USES:
   use LIS_surfaceModelDataMod, only : LIS_sfmodel_struc
   use LIS_coreMod
   use LIS_timeMgrMod,   only : LIS_clock, LIS_calendar, &
        LIS_update_timestep, LIS_registerAlarm
   use LIS_logMod,       only : LIS_verify, LIS_logunit
   use shr_sys_mod , only : shr_sys_flush
! !DESCRIPTION:
! Subroutine to read a global 2D variable from LDT into local variable, 1-D grid space.
!
!EOP
   implicit none
   integer, intent(in) :: n
   real(r8), pointer :: var(:)
   character(len=*), intent(in) :: varname
   logical, optional ::  readvar 
   integer :: t, g, gid, lastg, g0, i, j, ic0, ir0, gid0, lastgrid  ! how many tiles up to last grid

    integer                    :: ios
    integer                    :: ncId, nrId
    integer                    :: ncbId, nrbId
    integer                    :: varid, latId,lonId
    integer                    :: latbId,lonbId
    integer                    :: gnc,gnr
    integer                    :: gnc_b, gnr_b
    integer                    :: ftn
    integer                    :: k, gindex
    logical                    :: file_exists
    real,      allocatable     :: globalvar(:,:)
    real    :: localvar(LIS_rc%lnc(n),LIS_rc%lnr(n))

#if (defined USE_NETCDF3 || defined USE_NETCDF4)

       write(LIS_logunit, *) "Reading surface parameter: ", trim(varname) 

          ios = nf90_open(path=LIS_rc%paramfile(n),&
               mode=NF90_NOWRITE,ncid=ftn)
          call LIS_verify(ios,'Error in nf90_open in read_clm45_params:paramfile')

          ios = nf90_inq_dimid(ftn,"east_west",ncId)
          call LIS_verify(ios,'Error in nf90_inq_dimid in read_clm45_params:east_west')

          ios = nf90_inq_dimid(ftn,"north_south",nrId)
          call LIS_verify(ios,'Error in nf90_inq_dimid in read_clm45_params:north_south')

          ios = nf90_inquire_dimension(ftn,ncId, len=gnc)
          call LIS_verify(ios,'Error in nf90_inquire_dimension in read_clm45_params:ncId')

          ios = nf90_inquire_dimension(ftn,nrId, len=gnr)
          call LIS_verify(ios,'Error in nf90_inquire_dimension in read_clm45_params:nrId')

          allocate(globalvar(gnc,gnr))

          ios = nf90_inq_varid(ftn, trim(varname), varid)
          call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')

          ios = nf90_get_var(ftn, varid, globalvar)
          call LIS_verify(ios,'Error in nf90_get_var for '// trim(varname) // ' in read_clm45_params')

          ios = nf90_close(ftn)
          call LIS_verify(ios,'Error in nf90_close in read_clm45_params')

          localvar(:,:) = &
            globalvar(LIS_ews_halo_ind(n,LIS_localPet+1):&
            LIS_ewe_halo_ind(n,LIS_localPet+1), &
            LIS_nss_halo_ind(n,LIS_localPet+1): &
            LIS_nse_halo_ind(n,LIS_localPet+1))

          do t = 1, LIS_rc%ntiles(n)
           g = LIS_domain(n)%tile(t)%index
           var(g) = localvar(LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row)
           ! write(LIS_logunit, *) "g=", g, " var(g)= ", var(g)  
          end do
          call shr_sys_flush(LIS_logunit)
          deallocate(globalvar)
          readvar = .true. 
#endif

  end subroutine read_clm45_param_to_local_g1d

!BOP
!
! !ROUTINE: read_clm45_param_to_local_g2d
! \label{read_clm45_param_to_local_g2d}
!
! !INTERFACE:
  subroutine read_clm45_param_to_local_g2d(n, var, varname, zdimname, zsindex, readvar)
! !USES:
   use LIS_surfaceModelDataMod, only : LIS_sfmodel_struc
   use LIS_coreMod
   use LIS_timeMgrMod,   only : LIS_clock, LIS_calendar, &
        LIS_update_timestep, LIS_registerAlarm
   use LIS_logMod,       only : LIS_verify, LIS_logunit
! !DESCRIPTION:
! Subroutine to read a global 2D variable from LDT into local variable, 1-D grid space.
!
!EOP
   implicit none
   integer, intent(in) :: n
   real(r8), pointer :: var(:, :)
   character(len=*), intent(in) :: varname, zdimname
   integer, optional :: zsindex 
   logical, optional :: readvar 
   integer :: t, g, gid, lastg, g0, i, j, ic0, ir0, gid0, lastgrid  ! how many tiles up to last grid

    integer                    :: ios
    integer                    :: ncId, nrId, nzId
    integer                    :: ncbId, nrbId
    integer                    :: varid, latId,lonId
    integer                    :: latbId,lonbId
    integer                    :: gnc,gnr, gnz
    integer                    :: gnc_b, gnr_b
    integer                    :: ftn
    integer                    :: k, gindex
    logical                    :: file_exists
    real,      allocatable     :: globalvar(:, :,:)
    real,      allocatable     :: localvar(:, :, :)

   if ( .not. present(zsindex) ) zsindex = 1
#if (defined USE_NETCDF3 || defined USE_NETCDF4)

          ios = nf90_open(path=LIS_rc%paramfile(n),&
               mode=NF90_NOWRITE,ncid=ftn)
          call LIS_verify(ios,'Error in nf90_open in read_clm45_params:paramfile')

          ios = nf90_inq_dimid(ftn,"east_west",ncId)
          call LIS_verify(ios,'Error in nf90_inq_dimid in read_clm45_params:east_west')

          ios = nf90_inq_dimid(ftn,"north_south",nrId)
          call LIS_verify(ios,'Error in nf90_inq_dimid in read_clm45_params:north_south')

          ios = nf90_inq_dimid(ftn, trim(zdimname),nzId)
          call LIS_verify(ios,'Error in nf90_inq_dimid in read_clm45_params:'//trim(zdimname))

          ios = nf90_inquire_dimension(ftn,ncId, len=gnc)
          call LIS_verify(ios,'Error in nf90_inquire_dimension in read_clm45_params:ncId')

          ios = nf90_inquire_dimension(ftn,nrId, len=gnr)
          call LIS_verify(ios,'Error in nf90_inquire_dimension in read_clm45_params:nrId')

          ios = nf90_inquire_dimension(ftn,nzId, len=gnz)
          call LIS_verify(ios,'Error in nf90_inquire_dimension in read_clm45_params:nzId')

          if( zsindex .eq. 0 ) then 
            allocate(globalvar(gnc, gnr, 0:gnz-1))
            allocate(localvar(LIS_rc%lnc(n),LIS_rc%lnr(n), 0:gnz-1))
          else 
            allocate(globalvar(gnc, gnr, gnz))
            allocate(localvar(LIS_rc%lnc(n),LIS_rc%lnr(n), gnz))
          end if 

          ios = nf90_inq_varid(ftn, trim(varname), varid)
          call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')

          ios = nf90_get_var(ftn, varid, globalvar)
          call LIS_verify(ios,'Error in nf90_get_var for '// trim(varname) // ' in read_clm45_params')

          ios = nf90_close(ftn)
          call LIS_verify(ios,'Error in nf90_close in read_clm45_params')

          localvar(:,:, :) = &
            globalvar(LIS_ews_halo_ind(n,LIS_localPet+1):&
            LIS_ewe_halo_ind(n,LIS_localPet+1), &
            LIS_nss_halo_ind(n,LIS_localPet+1): &
            LIS_nse_halo_ind(n,LIS_localPet+1), :)

          do t = 1, LIS_rc%ntiles(n)
           g = LIS_domain(n)%tile(t)%index
           var(g, :) = localvar(LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row, :)
          end do
          deallocate(globalvar)
          deallocate(localvar)
          readvar = .true.
#endif

  end subroutine read_clm45_param_to_local_g2d

!BOP
!
! !ROUTINE: read_clm45_param_to_local_g1d_int
! \label{read_clm45_param_to_local_g1d_int}
!
! !INTERFACE:
  subroutine read_clm45_param_to_local_g1d_int(n, var, varname, readvar)
! !USES:
   use LIS_surfaceModelDataMod, only : LIS_sfmodel_struc
   use LIS_coreMod
   use LIS_timeMgrMod,   only : LIS_clock, LIS_calendar, &
        LIS_update_timestep, LIS_registerAlarm
   use LIS_logMod,       only : LIS_verify, LIS_logunit
! !DESCRIPTION:
! Subroutine to read a global 2D variable from LDT into local variable, 1-D grid space.
!
!EOP
   implicit none
   integer, intent(in) :: n
   integer, pointer :: var(:)
   character(len=*), intent(in) :: varname
   logical, optional :: readvar 
   integer :: t, g, gid, lastg, g0, i, j, ic0, ir0, gid0, lastgrid  ! how many tiles up to last grid

    integer                    :: ios
    integer                    :: ncId, nrId
    integer                    :: ncbId, nrbId
    integer                    :: varid, latId,lonId
    integer                    :: latbId,lonbId
    integer                    :: gnc,gnr
    integer                    :: gnc_b, gnr_b
    integer                    :: ftn
    integer                    :: k, gindex
    logical                    :: file_exists
    real,      allocatable     :: globalvar(:,:)
    real    :: localvar(LIS_rc%lnc(n),LIS_rc%lnr(n))

#if (defined USE_NETCDF3 || defined USE_NETCDF4)

          ios = nf90_open(path=LIS_rc%paramfile(n),&
               mode=NF90_NOWRITE,ncid=ftn)
          call LIS_verify(ios,'Error in nf90_open in read_clm45_params:paramfile')

          ios = nf90_inq_dimid(ftn,"east_west",ncId)
          call LIS_verify(ios,'Error in nf90_inq_dimid in read_clm45_params:east_west')

          ios = nf90_inq_dimid(ftn,"north_south",nrId)
          call LIS_verify(ios,'Error in nf90_inq_dimid in read_clm45_params:north_south')

          ios = nf90_inquire_dimension(ftn,ncId, len=gnc)
          call LIS_verify(ios,'Error in nf90_inquire_dimension in read_clm45_params:ncId')

          ios = nf90_inquire_dimension(ftn,nrId, len=gnr)
          call LIS_verify(ios,'Error in nf90_inquire_dimension in read_clm45_params:nrId')

          allocate(globalvar(gnc,gnr))

          ios = nf90_inq_varid(ftn, trim(varname), varid)
          call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')

          ios = nf90_get_var(ftn, varid, globalvar)
          call LIS_verify(ios,'Error in nf90_get_var for '// trim(varname) // ' in read_clm45_params')

          ios = nf90_close(ftn)
          call LIS_verify(ios,'Error in nf90_close in read_clm45_params')

          localvar(:,:) = &
            globalvar(LIS_ews_halo_ind(n,LIS_localPet+1):&
            LIS_ewe_halo_ind(n,LIS_localPet+1), &
            LIS_nss_halo_ind(n,LIS_localPet+1): &
            LIS_nse_halo_ind(n,LIS_localPet+1))

          do t = 1, LIS_rc%ntiles(n)
           g = LIS_domain(n)%tile(t)%index
           var(g) = nint(localvar(LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row))
          end do
          deallocate(globalvar)
          readvar = .true. 
#endif

  end subroutine read_clm45_param_to_local_g1d_int

end module clm45_read_paramsMod

