module organicFileMod

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: organicFileMod
!
! !DESCRIPTION:
! Contains methods for reading in organic matter data file which has 
! organic matter density for each grid point and soil level 
!
! !USES
  use abortutils   , only : endrun
  use clm_varctl   , only : iulog
  use shr_kind_mod , only : r8 => shr_kind_r8
!
! !PUBLIC TYPES:
  implicit none
  private
  save
!
! !PUBLIC MEMBER FUNCTIONS:
  public :: organicrd  ! Read organic matter dataset
!
! !REVISION HISTORY:
! Created by David Lawrence, 4 May 2006
! Revised by David Lawrence, 21 September 2007
! Revised by David Lawrence, 14 October 2008
!
!EOP
!
!----------------------------------------------------------------------- 

contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: organicrd
!
! !INTERFACE:
  subroutine organicrd(organic)
!
! !DESCRIPTION: 
! Read the organic matter dataset.
!
! !USES:
    use clm_varctl  , only : fsurdat, single_column
!YDT    use fileutils   , only : getfil
    use spmdMod     , only : masterproc
    use clmtype     , only : grlnd
    use domainMod   , only : ldomain
!YDT    use ncdio_pio
!
! !ARGUMENTS:
    implicit none
    real(r8), pointer :: organic(:,:)         ! organic matter density (kg/m3)
!
! !CALLED FROM:
! subroutine initialize in module initializeMod
!
! !REVISION HISTORY:
! Created by David Lawrence, 4 May 2006
! Revised by David Lawrence, 21 September 2007
!
!
! !LOCAL VARIABLES:
!EOP
    integer            :: nest               ! nest 

    character(len=256) :: locfn                 ! local file name
    integer            :: ni,nj,ns              ! dimension sizes  
    logical            :: isgrid2d              ! true => file is 2d
    logical            :: readvar               ! true => variable is on dataset
    character(len=32)  :: subname = 'organicrd' ! subroutine name
!-----------------------------------------------------------------------

    ! Initialize data to zero - no organic matter dataset

    organic(:,:)   = 0._r8
       
    ! Read data if file was specified in namelist
       
    if (fsurdat /= ' ') then
       if (masterproc) then
          write(iulog,*) 'Attempting to read organic matter data .....'
	  write(iulog,*) subname,trim(fsurdat)
       end if

       call read_clm45_organic_to_local_g2d(nest, organic, 'ORGANIC', 'nlevsoi', zsindex=1)

       if ( masterproc )then
          write(iulog,*) 'Successfully read organic matter data'
          write(iulog,*)
       end if
    endif

  end subroutine organicrd

!BOP
!
! !ROUTINE: read_clm45_organic_to_local_g2d
! \label{read_clm45_organic_to_local_g2d}
!
! !INTERFACE:
  subroutine read_clm45_organic_to_local_g2d(n, var, varname, zdimname, zsindex)
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
#endif

  end subroutine read_clm45_organic_to_local_g2d

end module organicFileMod
