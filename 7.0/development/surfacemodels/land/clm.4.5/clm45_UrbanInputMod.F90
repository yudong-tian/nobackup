module clm45_UrbanInputMod

!----------------------------------------------------------------------- 
!BOP
!
! !MODULE: UrbanInputMod
! 
! !DESCRIPTION: 
! Read in input urban data - fill in data structure urbinp
!
! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils  , only : endrun  
  use shr_sys_mod , only : shr_sys_flush 
  use LIS_logMod,       only : LIS_verify, LIS_logunit

  use netcdf
!
! !PUBLIC TYPES:
  implicit none
  save

  private
!
! !PUBLIC MEMBER FUNCTIONS:
  public :: clm45_UrbanInput         ! Read in urban input data

  type urbinp_t
     real(r8), pointer :: canyon_hwr(:,:)
     real(r8), pointer :: wtlunit_roof(:,:)
     real(r8), pointer :: wtroad_perv(:,:)
     real(r8), pointer :: em_roof(:,:)
     real(r8), pointer :: em_improad(:,:)
     real(r8), pointer :: em_perroad(:,:)
     real(r8), pointer :: em_wall(:,:)
     real(r8), pointer :: alb_roof_dir(:,:,:)
     real(r8), pointer :: alb_roof_dif(:,:,:)
     real(r8), pointer :: alb_improad_dir(:,:,:)
     real(r8), pointer :: alb_improad_dif(:,:,:)
     real(r8), pointer :: alb_perroad_dir(:,:,:)
     real(r8), pointer :: alb_perroad_dif(:,:,:)
     real(r8), pointer :: alb_wall_dir(:,:,:)
     real(r8), pointer :: alb_wall_dif(:,:,:)
     real(r8), pointer :: ht_roof(:,:)
     real(r8), pointer :: wind_hgt_canyon(:,:)
     real(r8), pointer :: tk_wall(:,:,:)
     real(r8), pointer :: tk_roof(:,:,:)
     real(r8), pointer :: tk_improad(:,:,:)
     real(r8), pointer :: cv_wall(:,:,:)
     real(r8), pointer :: cv_roof(:,:,:)
     real(r8), pointer :: cv_improad(:,:,:)
     real(r8), pointer :: thick_wall(:,:)
     real(r8), pointer :: thick_roof(:,:)
     integer,  pointer :: nlev_improad(:,:)
     real(r8), pointer :: t_building_min(:,:)
     real(r8), pointer :: t_building_max(:,:)
  end type urbinp_t

  type (urbinp_t)   , public :: urbinp        ! urban input derived type
!
!EOP
!-----------------------------------------------------------------------

contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: clm45_UrbanInput
!
! !INTERFACE:
  subroutine clm45_UrbanInput(nn, mode)
!
! !DESCRIPTION: 
! Allocate memory and read in urban input data
!
! !USES:
    use clm_varpar, only : numrad, nlevurb, numurbl
    use clm_varctl, only : fsurdat, single_column
    use spmdMod   , only : masterproc
    use clmtype ,   only : grlnd
    use clm45_decompMod , only : get_proc_bounds
    use domainMod , only : ldomain
    use LIS_coreMod 
!
! !ARGUMENTS:
    implicit none
    integer, intent(in) :: nn  ! nest 
    character(len=*), intent(in) :: mode
!
! !CALLED FROM:
! subroutine initialize
!
! !REVISION HISTORY:
! Created by Mariana Vertenstein July 2004
! Revised by Keith Oleson for netcdf input Jan 2008
!
!
! !LOCAL VARIABLES:
!EOP
    character(len=256) :: locfn      ! local file name
    integer  :: ncid       ! netcdf id
    integer :: dimid                 ! netCDF id
    integer :: begg,endg             ! start/stop gridcells
    integer :: nw,n,k,i,j,ni,nj,ns   ! indices
    integer :: nlevurb_i             ! input grid: number of urban vertical levels
    integer :: numrad_i              ! input grid: number of solar bands (VIS/NIR)
    integer :: numurbl_i             ! input grid: number of urban landunits
    integer :: ier,ret,ios           ! error status
    logical :: isgrid2d              ! true => file is 2d
    logical :: readvar               ! true => variable is on dataset
    logical :: has_numurbl           ! true => numurbl dimension is on dataset
    character(len=32) :: subname = 'clm45_UrbanInput' ! subroutine name

!-----------------------------------------------------------------------

    if ( nlevurb == 0 ) return

    call get_proc_bounds(begg,endg)

    if (mode == 'initialize') then

       ! Read urban data

       if (masterproc) then
          write(LIS_logunit,*)' Reading in urban input data from fsurdat file ...'
       end if

       allocate(urbinp%canyon_hwr(begg:endg,numurbl), &
                urbinp%wtlunit_roof(begg:endg,numurbl), &
                urbinp%wtroad_perv(begg:endg,numurbl), &
                urbinp%em_roof(begg:endg,numurbl), &
                urbinp%em_improad(begg:endg,numurbl), &
                urbinp%em_perroad(begg:endg,numurbl), &
                urbinp%em_wall(begg:endg,numurbl), &
                urbinp%alb_roof_dir(begg:endg,numurbl,numrad), &
                urbinp%alb_roof_dif(begg:endg,numurbl,numrad), &
                urbinp%alb_improad_dir(begg:endg,numurbl,numrad), &
                urbinp%alb_perroad_dir(begg:endg,numurbl,numrad), &
                urbinp%alb_improad_dif(begg:endg,numurbl,numrad), &
                urbinp%alb_perroad_dif(begg:endg,numurbl,numrad), &
                urbinp%alb_wall_dir(begg:endg,numurbl,numrad), &
                urbinp%alb_wall_dif(begg:endg,numurbl,numrad), &
                urbinp%ht_roof(begg:endg,numurbl), &
                urbinp%wind_hgt_canyon(begg:endg,numurbl), &
                urbinp%tk_wall(begg:endg,numurbl,nlevurb), &
                urbinp%tk_roof(begg:endg,numurbl,nlevurb), &
                urbinp%tk_improad(begg:endg,numurbl,nlevurb), &
                urbinp%cv_wall(begg:endg,numurbl,nlevurb), &
                urbinp%cv_roof(begg:endg,numurbl,nlevurb), &
                urbinp%cv_improad(begg:endg,numurbl,nlevurb), &
                urbinp%thick_wall(begg:endg,numurbl), &
                urbinp%thick_roof(begg:endg,numurbl), &
                urbinp%nlev_improad(begg:endg,numurbl), &
                urbinp%t_building_min(begg:endg,numurbl), &
                urbinp%t_building_max(begg:endg,numurbl), &
                stat=ier)
       if (ier /= 0) then
          write(LIS_logunit,*)'initUrbanInput: allocation error '; call endrun()
       endif

       ! Read urban data
       
       if (masterproc) then
          write(LIS_logunit,*)' Reading in urban input data from fsurdat file ...'
       end if
       
      ios = nf90_open(path=LIS_rc%paramfile(n), mode=NF90_NOWRITE, ncid=ncid)
      call LIS_verify(ios,'Error in nf90_open in Urbaninit:paramfile')

       !2D: begg:endg,numurbl  

       call clm45_urban_read_g2d(ncid=ncid, varname='CANYON_HWR', flag='read', data=urbinp%canyon_hwr,&
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: CANYON_HWR NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='WTLUNIT_ROOF', flag='read', data=urbinp%wtlunit_roof, &
             readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: WTLUNIT_ROOF NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='WTROAD_PERV', flag='read', data=urbinp%wtroad_perv, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: WTROAD_PERV NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='EM_ROOF', flag='read', data=urbinp%em_roof, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: EM_ROOF NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='EM_IMPROAD', flag='read', data=urbinp%em_improad, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: EM_IMPROAD NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='EM_PERROAD', flag='read', data=urbinp%em_perroad, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: EM_PERROAD NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='EM_WALL', flag='read', data=urbinp%em_wall, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: EM_WALL NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='HT_ROOF', flag='read', data=urbinp%ht_roof, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: HT_ROOF NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='WIND_HGT_CANYON', flag='read', data=urbinp%wind_hgt_canyon, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: WIND_HGT_CANYON NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='THICK_WALL', flag='read', data=urbinp%thick_wall, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: THICK_WALL NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='THICK_ROOF', flag='read', data=urbinp%thick_roof, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: THICK_ROOF NOT on fsurdat file' )

       call clm45_urban_read_int_g2d(ncid=ncid, varname='NLEV_IMPROAD', flag='read', data=urbinp%nlev_improad, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: NLEV_IMPROAD NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='T_BUILDING_MIN', flag='read', data=urbinp%t_building_min, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: T_BUILDING_MIN NOT on fsurdat file' )

       call clm45_urban_read_g2d(ncid=ncid, varname='T_BUILDING_MAX', flag='read', data=urbinp%t_building_max, &
            readvar=readvar, z1dim=numurbl)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: T_BUILDING_MAX NOT on fsurdat file' )

       ! 3D: begg:endg,numurbl,numrad or begg:endg,numurbl,nlevurb)

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_IMPROAD_DIR', flag='read', data=urbinp%alb_improad_dir, &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not.readvar) call endrun( trim(subname)//' ERROR: ALB_IMPROAD_DIR NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_IMPROAD_DIF', flag='read', data=urbinp%alb_improad_dif, &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not.readvar) call endrun( trim(subname)//' ERROR: ALB_IMPROAD_DIF NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_PERROAD_DIR', flag='read',data=urbinp%alb_perroad_dir, &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: ALB_PERROAD_DIR NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_PERROAD_DIF', flag='read',data=urbinp%alb_perroad_dif, &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: ALB_PERROAD_DIF NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_ROOF_DIR', flag='read', data=urbinp%alb_roof_dir,  &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: ALB_ROOF_DIR NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_ROOF_DIF', flag='read', data=urbinp%alb_roof_dif,  &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: ALB_ROOF_DIF NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_WALL_DIR', flag='read', data=urbinp%alb_wall_dir, &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: ALB_WALL_DIR NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='ALB_WALL_DIF', flag='read', data=urbinp%alb_wall_dif, &
            readvar=readvar, z1dim=numurbl, z2dim=numrad)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: ALB_WALL_DIF NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='TK_IMPROAD', flag='read', data=urbinp%tk_improad, &
            readvar=readvar, z1dim=numurbl, z2dim=nlevurb)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: TK_IMPROAD NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='TK_ROOF', flag='read', data=urbinp%tk_roof, &
            readvar=readvar, z1dim=numurbl, z2dim=nlevurb)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: TK_ROOF NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='TK_WALL', flag='read', data=urbinp%tk_wall, &
            readvar=readvar, z1dim=numurbl, z2dim=nlevurb)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: TK_WALL NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='CV_IMPROAD', flag='read', data=urbinp%cv_improad, &
            readvar=readvar, z1dim=numurbl, z2dim=nlevurb)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: CV_IMPROAD NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='CV_ROOF', flag='read', data=urbinp%cv_roof, &
            readvar=readvar, z1dim=numurbl, z2dim=nlevurb)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: CV_ROOF NOT on fsurdat file' )

       call clm45_urban_read_g3d(ncid=ncid, varname='CV_WALL', flag='read', data=urbinp%cv_wall, &
            readvar=readvar, z1dim=numurbl, z2dim=nlevurb)
       if (.not. readvar) call endrun( trim(subname)//' ERROR: CV_WALL NOT on fsurdat file' )

       ios=nf90_close(ncid)  

       if (masterproc) then
          write(LIS_logunit,*)' Sucessfully read urban input data' 
          write(LIS_logunit,*)
       end if

    else if (mode == 'finalize') then

        if ( nlevurb == 0 ) return

       deallocate(urbinp%canyon_hwr, &
                  urbinp%wtlunit_roof, &
                  urbinp%wtroad_perv, &
                  urbinp%em_roof, &
                  urbinp%em_improad, &
                  urbinp%em_perroad, &
                  urbinp%em_wall, &
                  urbinp%alb_roof_dir, &
                  urbinp%alb_roof_dif, &
                  urbinp%alb_improad_dir, &
                  urbinp%alb_perroad_dir, &
                  urbinp%alb_improad_dif, &
                  urbinp%alb_perroad_dif, &
                  urbinp%alb_wall_dir, &
                  urbinp%alb_wall_dif, &
                  urbinp%ht_roof, &
                  urbinp%wind_hgt_canyon, &
                  urbinp%tk_wall, &
                  urbinp%tk_roof, &
                  urbinp%tk_improad, &
                  urbinp%cv_wall, &
                  urbinp%cv_roof, &
                  urbinp%cv_improad, &
                  urbinp%thick_wall, &
                  urbinp%thick_roof, &
                  urbinp%nlev_improad, &
                  urbinp%t_building_min, &
                  urbinp%t_building_max, &
                  stat=ier)

       if (ier /= 0) then
          write(LIS_logunit,*)'initUrbanInput: deallocation error '; call endrun()
       endif

    else
       write(LIS_logunit,*)'initUrbanInput error: mode ',trim(mode),' not supported '
       call endrun()
    end if

  end subroutine clm45_UrbanInput


!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: clm45_urban_read_g2d
!
! !INTERFACE:
  subroutine clm45_urban_read_g2d(varname, data, flag, ncid, readvar, nt, posNOTonfile, z1dim)
!
! !DESCRIPTION:
! netcdf I/O of global
! YDT: rewritten from clm45_urban_read_g1d_int_var0_nf
   use LIS_coreMod 
   use LIS_domainMod 
!
! !ARGUMENTS:
    implicit none
    integer, intent(inout) :: ncid      ! netcdf file id
    character(len=*) , intent(in)    :: flag      ! 'read' or 'write'
    character(len=*) , intent(in)    :: varname   ! variable name
    real(r8)         , intent(inout) :: data(:, :)   ! raw data
    logical, optional, intent(out)   :: readvar   ! was var read?
    integer, optional, intent(in)    :: nt        ! time sample index
    logical          , optional, intent(in) :: posNOTonfile ! position is NOT on this file
    integer, optional, intent(in)    :: z1dim        ! z dim 
    integer  :: varid, ios
    integer :: t, g
!
    integer, parameter  :: n = 1

    real(r8), allocatable :: localvar(:, :, :), globalvar(:, :, :) 

    allocate(localvar(LIS_rc%lnc(n),LIS_rc%lnr(n), z1dim) ) 
    allocate(globalvar(LIS_rc%gnc(n),LIS_rc%gnr(n), z1dim) ) 


    ios = nf90_inq_varid(ncid, trim(varname), varid)
    call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')
    ios = nf90_get_var(ncid, varid, globalvar)
    call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')
    readvar = .true.

          localvar(:,:, :) = &
            globalvar(LIS_ews_halo_ind(n,LIS_localPet+1):&
            LIS_ewe_halo_ind(n,LIS_localPet+1), &
            LIS_nss_halo_ind(n,LIS_localPet+1): &
            LIS_nse_halo_ind(n,LIS_localPet+1), :)

          do t = 1, LIS_rc%ntiles(n)
           g = LIS_domain(n)%tile(t)%index
           data(g, :) = localvar(LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row, :)
          end do

     deallocate(localvar) 
     deallocate(globalvar) 

    end subroutine clm45_urban_read_g2d


!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: clm45_urban_read_int_g2d
!
! !INTERFACE:
  subroutine clm45_urban_read_int_g2d(varname, data, flag, ncid, readvar, nt, posNOTonfile, z1dim)
!
! !DESCRIPTION:
! netcdf I/O of global
! YDT: rewritten from clm45_urban_read_g1d_int_var0_nf
   use LIS_coreMod 
   use LIS_domainMod 
!
! !ARGUMENTS:
    implicit none
    integer, intent(inout) :: ncid      ! netcdf file id
    character(len=*) , intent(in)    :: flag      ! 'read' or 'write'
    character(len=*) , intent(in)    :: varname   ! variable name
    integer         , intent(inout) :: data(:, :)   ! raw data
    logical, optional, intent(out)   :: readvar   ! was var read?
    integer, optional, intent(in)    :: nt        ! time sample index
    logical          , optional, intent(in) :: posNOTonfile ! position is NOT on this file
    integer, optional, intent(in)    :: z1dim        ! z dim 
    integer  :: varid, ios
    integer :: t, g
!
 
    integer, parameter  :: n = 1

    integer, allocatable :: localvar(:, :, :), globalvar(:, :, :) 

    allocate(localvar(LIS_rc%lnc(n),LIS_rc%lnr(n), z1dim) ) 
    allocate(globalvar(LIS_rc%gnc(n),LIS_rc%gnr(n), z1dim) ) 


    ios = nf90_inq_varid(ncid, trim(varname), varid)
    call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')
    ios = nf90_get_var(ncid, varid, globalvar)
    call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')
    readvar = .true.

          localvar(:,:, :) = &
            globalvar(LIS_ews_halo_ind(n,LIS_localPet+1):&
            LIS_ewe_halo_ind(n,LIS_localPet+1), &
            LIS_nss_halo_ind(n,LIS_localPet+1): &
            LIS_nse_halo_ind(n,LIS_localPet+1), :)

          do t = 1, LIS_rc%ntiles(n)
           g = LIS_domain(n)%tile(t)%index
           data(g, :) = localvar(LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row, :)
          end do

     deallocate(localvar) 
     deallocate(globalvar) 

    end subroutine clm45_urban_read_int_g2d


!------------------------------------------------------------------------
!BOP
!
! !IROUTINE: clm45_urban_read_g3d
!
! !INTERFACE:
  subroutine clm45_urban_read_g3d(varname, data, flag, ncid, readvar, nt, posNOTonfile, z1dim, z2dim)
!
! !DESCRIPTION:
! netcdf I/O of global
! YDT: rewritten from clm45_urban_read_g1d_int_var0_nf
   use LIS_coreMod 
   use LIS_domainMod
!
! !ARGUMENTS:
    implicit none
    integer, intent(inout) :: ncid      ! netcdf file id
    character(len=*) , intent(in)    :: flag      ! 'read' or 'write'
    character(len=*) , intent(in)    :: varname   ! variable name
    real(r8)         , intent(inout) :: data(:, :, :) ! raw data
    logical, optional, intent(out)   :: readvar   ! was var read?
    integer, optional, intent(in)    :: nt        ! time sample index
    logical          , optional, intent(in) :: posNOTonfile ! position is NOT on this file
    integer, optional, intent(in)    :: z1dim, z2dim   ! two z dims 
    integer  :: varid, ios
    integer :: t, g

    integer, parameter  :: n = 1

    real(r8),      allocatable     :: globalvar(:, :, :, :), localvar(:, :, :, :) 

    allocate(localvar(LIS_rc%lnc(n),LIS_rc%lnr(n), z1dim, z2dim) ) 
    allocate(globalvar(LIS_rc%gnc(n),LIS_rc%gnr(n), z1dim, z2dim) ) 


    ios = nf90_inq_varid(ncid, trim(varname), varid)
    call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')
    ios = nf90_get_var(ncid, varid, globalvar)
    call LIS_verify(ios, trim(varname)// ' field not found in the LIS param file')
    readvar = .true.

          localvar(:, :, :, :) = &
            globalvar(LIS_ews_halo_ind(n,LIS_localPet+1):&
            LIS_ewe_halo_ind(n,LIS_localPet+1), &
            LIS_nss_halo_ind(n,LIS_localPet+1): &
            LIS_nse_halo_ind(n,LIS_localPet+1), :, :)

          do t = 1, LIS_rc%ntiles(n)
           g = LIS_domain(n)%tile(t)%index
           data(g, :, :) = localvar(LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row, :, :)
          end do

    deallocate(localvar) 
    deallocate(globalvar) 

    end subroutine clm45_urban_read_g3d

end module clm45_UrbanInputMod

