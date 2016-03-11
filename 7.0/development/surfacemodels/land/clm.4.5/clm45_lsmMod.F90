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
! 24 Feb 2016; Yudong Tian, initial implementation of clm4.5, without nesting support. 
!
! !USES:        

    use netcdf
  !CLM45 native modules
    use clmtypeInitMod  , only : initClmtype
    use clmtype
    use clm_varpar      , only : maxpatch, clm_varpar_init
    use clm_varcon      , only : clm_varcon_init
    use clm_varctl      , only : fsurdat, fatmlndfrc, flndtopo, fglcmask, noland, &
                                 create_glacier_mec_landunit, allocate_all_vegpfts, &
				 create_crop_landunit, single_column
    use clm45_pftvarcon       , only : clm45_pftconrd
    use clm45_surfrdMod
    use clm_atmlnd   ,   only : atm2lnd_type 
    use clm45_decompInitMod   , only : decompInit_lnd, decompInit_glcp
    use clm45_decompMod       
    use domainMod       , only : domain_check, ldomain, domain_init
    use spmdMod         , only : masterproc
    use clm45_UrbanInputMod

  ! LIS modules
  use clm45_module
  use LIS_logMod

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
  public :: clm45_domain 
  public :: clm45_struc

!EOP

!------------ beginning of domain  -------------------
 type, public :: clm45_domain_dec
  integer,public :: nclumps     ! total number of clumps across all processors
  integer,public :: numg        ! total number of gridcells on all procs
  integer,public :: numl        ! total number of landunits on all procs
  integer,public :: numc        ! total number of columns on all procs
  integer,public :: nump        ! total number of pfts on all procs
  type(processor_type),public :: procinfo
  type(clump_type),public, allocatable :: clumps(:)
  type(decomp_type),public :: ldecomp
  type(domain_type), public :: ldomain
 end type clm45_domain_dec

 type(clm45_domain_dec), allocatable :: clm45_domain(:)

!------------ end of domain stuff -------------------

!============== state variables ======================

  type, public :: clm45_type_dec
     character*100 :: clm_rfile
     character*256 :: clm_vfile
     character*100 :: clm_chtfile
     integer      :: clm45open 
     integer      :: count
     integer                    :: clmopen
     integer                    :: numout
     real                       :: ts
     real                       :: rstInterval
! CLM4.5 native types
! extracted from  ~/CESM/cesm1_2_2_dev/models/lnd/clm/src/clm4_5/main/clmtype.F90 
type(energy_balance_type)   :: pebal !energy balance structure
type(energy_balance_type)   :: cebal !energy balance structure
type(water_balance_type)    :: pwbal !water balance structure
type(water_balance_type)    :: cwbal !water balance structure
type(carbon_balance_type)   :: pcbal !carbon balance structure
type(carbon_balance_type)   :: ccbal !carbon balance structure
type(nitrogen_balance_type) :: pnbal !nitrogen balance structure
type(nitrogen_balance_type) :: cnbal !nitrogen balance structure
type(pft_pstate_type) :: pps      !physical state variables
type(pft_pstate_type) :: pps_a    !pft-level pstate variables averaged to the column
type(pft_psynstate_type)::ppsyns     !photosynthesis relevant variables
type(pft_epc_type)  :: pftcon
type(decomp_cascade_type) :: decomp_cascade_con
type(pft_dgvepc_type) :: dgv_pftcon
type(pft_epv_type)    :: pepv        !pft ecophysiological variables
type(pft_estate_type) :: pes      !pft energy state
type(pft_wstate_type) :: pws         !pft water state
type(pft_cstate_type)  :: pcs         !pft carbon state
type(pft_cstate_type) :: pcs_a       !pft-level carbon state averaged to the column
type(pft_cstate_type) :: pc13s       !pft carbon-13 state
type(pft_cstate_type) :: pc13s_a     !pft carbon-13 state averaged to the column
type(pft_cstate_type) :: pc14s       !pft carbon-14 state
type(pft_cstate_type) :: pc14s_a     !pft carbon-14 state averaged to the column
type(pft_nstate_type) :: pns      !pft nitrogen state
type(pft_nstate_type) :: pns_a    !pft-level nitrogen state variables averaged to the column
type(pft_vstate_type) :: pvs         !pft VOC state
type(pft_dgvstate_type) :: pdgvs     !pft DGVM state variables
type(pft_eflux_type)  :: pef         !pft energy flux
type(pft_mflux_type)  :: pmf         !pft momentum flux
type(pft_wflux_type)  :: pwf         !pft water flux
type(pft_cflux_type)  :: pcf      !pft carbon flux
type(pft_cflux_type) :: pcf_a    !pft carbon flux averaged to the column
type(pft_cflux_type) :: pc13f    !pft carbon-13 flux
type(pft_cflux_type) :: pc13f_a  !pft carbon-13 flux averaged to the column
type(pft_cflux_type) :: pc14f    !pft carbon-14 flux
type(pft_cflux_type) :: pc14f_a  !pft carbon-14 flux averaged to the column
type(pft_nflux_type)  :: pnf       !pft nitrogen flux
type(pft_nflux_type)  :: pnf_a     !pft-level nitrogen flux variables averaged to the column
type(pft_vflux_type)  :: pvf         !pft VOC flux
type(pft_depvd_type)  :: pdd         !dry dep velocity
type(pft_dflux_type)  :: pdf         !pft dust flux
type(column_pstate_type) :: cps      !column physical state variables
type(column_estate_type) :: ces      !column energy state
type(column_wstate_type) :: cws      !column water state
type(pft_wstate_type)    :: pws_a    !pft-level water state variables averaged to the column
type(column_cstate_type) :: ccs      !column carbon state
type(column_cstate_type) :: cc13s    !column carbon-13 state
type(column_cstate_type) :: cc14s    !column carbon-14 state
type(column_ch4_type)   :: cch4      !column CH4 variables
type(column_nstate_type) :: cns      !column nitrogen state
type(column_nstate_type) :: cns_a    !column-level nitrogen state variables averaged to gridcell
type(column_eflux_type) :: cef       ! column energy flux
type(pft_eflux_type)    :: pef_a     ! pft-level energy flux variables averaged to the column
type(column_wflux_type) :: cwf       ! column water flux
type(pft_wflux_type)    :: pwf_a     ! pft-level water flux variables averaged to the column
type(column_cflux_type) :: ccf    ! column carbon flux
type(column_cflux_type) :: cc13f  ! column carbon-13 flux
type(column_cflux_type) :: cc14f  ! column carbon-14 flux
type(column_nflux_type) :: cnf       !column nitrogen flux
type(landunit_pstate_type) :: lps     !land unit physical state variables
type(column_pstate_type)   :: cps_a   !column-level physical state variables averaged to landunit
type(landunit_eflux_type) :: lef     ! average of energy fluxes all columns
type(column_eflux_type)   :: cef_a   ! column-level energy flux variables averaged to landunit
type(gridcell_estate_type) :: ges    !average of energy states all landunits
type(gridcell_wstate_type) :: gws    !average of water states all landunits
type(gridcell_ch4_type)   :: gch4    !average of CH4 fluxes all landunits
type(column_vstate_type):: cvs_a            !column-level VOC state variables averaged to gridcell
type(gridcell_efstate_type):: gve       !gridcell VOC emission factors
type(gridcell_dgvstate_type):: gdgvs !gridcell DGVM structure
type(gridcell_eflux_type) :: gef     !average of energy fluxes all landunits
type(gridcell_wflux_type) :: gwf     !average of water fluxes all landunits
type(pft_type) :: pft  !plant functional type (pft) data structure
type(column_type) :: col !column data structure (soil/snow/canopy columns)
type(landunit_type) :: lun  !geomorphological landunits
type(gridcell_type) :: grc    !gridcell data structure

!forcing, 
  type(atm2lnd_type) :: clm_a2l      ! a2l fields on clm grid

  end type clm45_type_dec

  type(clm45_type_dec), allocatable :: clm45_struc(:)   ! nnest copies 

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
   use LIS_surfaceModelDataMod, only : LIS_sfmodel_struc
   use LIS_coreMod
   use LIS_timeMgrMod,   only : LIS_clock, LIS_calendar, &
        LIS_update_timestep, LIS_registerAlarm
   use LIS_logMod,       only : LIS_verify, LIS_logunit
   use clm_varsur
! !DESCRIPTION:        
!
!EOP
   implicit none
   integer :: n, i, t, g, gic, gir, gid, git
   integer                 :: yr, mo, da, hr, mn, ss
   integer                 :: status
   character*3   :: fnest

   integer :: begg, endg, begl, endl, begc, endc, begp, endp
   integer :: ni, nj, ns 
   integer :: ier 

   masterproc = LIS_masterproc 
   
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
!           write(LIS_logunit,'(6I6, 2F8.2, 3I5, 2I7)') t, LIS_domain(n)%tile(t)%col, LIS_domain(n)%tile(t)%row, &
!                                   LIS_domain(n)%tile(t)%index, LIS_domain(n)%tile(t)%tile_id, &
!                                   LIS_domain(n)%tile(t)%vegt, &
!                                   LIS_domain(n)%grid(g)%lon, LIS_domain(n)%grid(g)%lat, & 
!                                   gic, gir,  LIS_domain(n)%ntiles_pergrid(gid), LIS_domain(n)%str_tind(gid), git
     end do 
   end do 
   write(LIS_logunit, *) ! empty line
                                 
   do n = 1, LIS_rc%nnest
     ni = LIS_ewe_halo_ind(n,LIS_localPet+1) - LIS_ews_halo_ind(n,LIS_localPet+1) + 1 
     nj = LIS_nse_halo_ind(n,LIS_localPet+1) - LIS_nss_halo_ind(n,LIS_localPet+1) + 1 
   end do 

   allocate(clm45_domain(LIS_rc%nnest))
   allocate(clm45_struc(LIS_rc%nnest))

   call clm45_readcrd()

   ! Setting control variables, used to be done in clmvarctl_init() 

     create_glacier_mec_landunit = .false.
     allocate_all_vegpfts = .true.  
     create_crop_landunit = .false. 
     single_column = .false.
     ! used to be in decompMod. number of clumps per cpu
     clump_pproc = 1

    ! Read list of PFTs and their corresponding parameter values
    ! Independent of model resolution, Needs to stay before surfrd_get_data


   do n = 1, LIS_rc%nnest

     call clm45_pftconrd(clm45_struc(n)%clm_vfile)  
     ! need to make nest-dependent (but they are  tile-independent variables and structures ) 
      call clm_varpar_init()   ! number of patches, vertical levels, etc
      call clm_varcon_init()   ! physical constants and thickness of vertical levels 
      ! translate LIS decomposed domain into CLM45 domain
      write(LIS_logunit,*) 'clm_varpar_init and clm_varcon_init done. '  
      call clm45_domain_init(n, ni, nj)

      ! check if can get grid and pft range now
      call get_proc_bounds (begg, endg, begl, endl, begc, endc, begp, endp)
      write(LIS_logunit,*) 'Local domain info: '  
      write(LIS_logunit,*) '  begg   endg   begl  endl   begc   endc   begp   endp  '
      write(LIS_logunit,*) '============================================================================' 
      write(LIS_logunit,'(8I7)') begg, endg, begl, endl, begc, endc, begp, endp
      write(LIS_logunit,*) 

      ! read other parameters need to create the full glcp suite
     call clm45_surfrd_get_grid(n, ldomain) 
     if (masterproc) then
       call domain_check(ldomain)
     endif

    ! Initialize urban model input (initialize urbinp data structure)
    call clm45_UrbanInput(n, mode='initialize')

    ! Allocate surface grid dynamic memory (for wtxy and vegxy arrays)
    ! Allocate additional dynamic memory for glacier_mec topo and thickness

    ! YDT: need to make nest-dependent 
    call get_proc_bounds(begg, endg)
    allocate (vegxy(begg:endg,maxpatch), wtxy(begg:endg,maxpatch), stat=ier)

!    if (create_glacier_mec_landunit) then
!       allocate (topoxy(begg:endg,maxpatch), stat=ier)
!    else
     allocate (topoxy(1,1), stat=ier)
!    endif

    ! Read surface dataset and set up vegetation type [vegxy] and
    ! weight [wtxy] arrays for [maxpatch] subgrid patches.

    ! YDT: need to make nest-dependent, rewrite io 
     call clm45_surfrd_get_data(n, ldomain) 

    ! Determine decomposition of subgrid scale landunits, columns, pfts

     write(LIS_logunit,*) '  ni    nj' 
     write(LIS_logunit,*) '=============='
     write(LIS_logunit,'(2I7)') ni, nj 
     write(LIS_logunit,*) 

     ! the equivalent of decompInit_lnd() is done inn clm45_domain_init() 
     call decompInit_glcp (n, ni*nj, ni, nj)

      ! allocate all the data structures for each nest
      !call init_energy_balance_type(begp, endp, clm45_struc(n)%pebal)
      !call init_energy_balance_type(begc, endc, clm45_struc(n)%cebal)
      ! ...

      call clm45_lsm_init(n)

!------------------------------------------------------------------------
! Model timestep Alarm
!------------------------------------------------------------------------
       call LIS_update_timestep(LIS_rc, n, clm45_struc(n)%ts)

       write(fnest,'(i3.3)') n

       call LIS_registerAlarm("CLM45 model alarm "//trim(fnest),&
            clm45_struc(n)%ts,&
            clm45_struc(n)%ts)

       call LIS_registerAlarm("CLM45 restart alarm "//trim(fnest),&
            clm45_struc(n)%ts,&
            clm45_struc(n)%rstInterval)

      clm45_struc(n)%numout = 0
      clm45_struc(n)%count = 0
      clm45_struc(n)%clmopen = 0


      LIS_sfmodel_struc(n)%nsm_layers = 1
      LIS_sfmodel_struc(n)%nst_layers = 1
      allocate(LIS_sfmodel_struc(n)%lyrthk(1))
      LIS_sfmodel_struc(n)%lyrthk(1) = 1
      LIS_sfmodel_struc(n)%ts = clm45_struc(n)%ts
   enddo
  
  end subroutine clm45_lsm_ini

!BOP
!
! !ROUTINE: clm45_domain_init
! \label{clm45_domain_init}
!
! !INTERFACE:
  subroutine clm45_domain_init(n, lni, lnj)
! !USES:
    use LIS_coreMod
    use LIS_precisionMod
 
    use clm45_decompMod
! !DESCRIPTION:
!
! Initializes clm45 domain for nest n 
!
!  The arguments are:
! \begin{description}
!  \item[n]
!   index of the nest
! \end{description}
!EOP
    integer :: n   ! nest 
    integer :: beg, end 
    integer :: lni,lnj,lns                     ! size of grid on file
    logical :: isgrid2d                     ! true => file is 2d lat/lon

    integer :: k, ai, aj, an, ag
    integer :: ier, ip, cid 

    ! with clump_pproc = 1, nclumps = LIS_npes; cid ranges from 1 ... LIS_npes, 
    ! with corresponding owner ranging from 0 ... LIS_npes-1 (pid)

    isgrid2d = .true. 
    lns = lni*lnj
    nclumps = clump_pproc * LIS_npes

    allocate(procinfo%cid(clump_pproc), stat=ier)

    procinfo%nclumps = clump_pproc
    procinfo%cid(:)  = -1
    procinfo%ncells  = 0
    procinfo%nlunits = 0
    procinfo%ncols   = 0
    procinfo%npfts   = 0
    procinfo%begg    = 1
    procinfo%begl    = 1
    procinfo%begc    = 1
    procinfo%begp    = 1
    procinfo%endg    = 0
    procinfo%endl    = 0
    procinfo%endc    = 0
    procinfo%endp    = 0

    allocate(clumps(nclumps), stat=ier)

    clumps(:)%owner   = -1
    clumps(:)%ncells  = 0
    clumps(:)%nlunits = 0
    clumps(:)%ncols   = 0
    clumps(:)%npfts   = 0
    clumps(:)%begg    = 1
    clumps(:)%begl    = 1
    clumps(:)%begc    = 1
    clumps(:)%begp    = 1
    clumps(:)%endg    = 0
    clumps(:)%endl    = 0
    clumps(:)%endc    = 0
    clumps(:)%endp    = 0

    Do ip = 0, LIS_npes-1 
       cid = ip+1
       clumps(cid)%owner   = ip 
       procinfo%cid(clump_pproc)  = cid 
    End Do 

    cid = LIS_localPet + 1
    ! cpu-local 
    if ( clumps(cid)%owner == LIS_localPet ) then 
       procinfo%ncells  = LIS_ngrids(n, LIS_localPet)
       procinfo%begg = 1    ! pe-local, every one starts from 1
       procinfo%endg = LIS_ngrids(n, LIS_localPet)   
       procinfo%begp = 1    ! pe-local, every one starts from 1
       procinfo%endp = LIS_ntiless(n, LIS_localPet)  
    end if 

    ! across cpu
    Do ip = 0, LIS_npes-1 
       cid = ip+1
       clumps(cid)%ncells  =  LIS_ngrids(n, ip) 
       clumps(cid)%begg  =  1 
       clumps(cid)%endg  =  LIS_ngrids(n, ip) 
       clumps(cid)%begp  =  1 
       clumps(cid)%endp  =  LIS_ntiless(n, ip) 
    End Do 

    call get_proc_bounds(beg, end)
    call domain_init(ldomain, isgrid2d=isgrid2d, ni=lni, nj=lnj, nbeg=beg, nend=end)

    numg = end - beg + 1
    allocate(ldecomp%gdc2glo(numg), ldecomp%glo2gdc(lns), stat=ier)
    if (ier /= 0) then
       write(LIS_logunit,*) 'decompInit_lnd(): allocation error1 for ldecomp, etc'
       call endrun()
    end if

    ldecomp%gdc2glo(:) = 0
    ldecomp%glo2gdc(:) = 0
!  gdc2glo: 1-d land grid index to index in (lnc, lnr) space
!  glo2gdc: opposite. 
    ag = 0
          do aj = 1,lnj
            do ai = 1,lni
                an = (aj-1)*lni + ai
                ag = ag + 1
                ldecomp%gdc2glo(ag) = an
                ldecomp%glo2gdc(an) = ag
            end do
          end do

 end subroutine clm45_domain_init


!BOP
!
! !ROUTINE: clm45_lsm_init
! \label{clm45_lsm_init}
!
! !INTERFACE:
  subroutine clm45_lsm_init(n)
! !USES:
    use LIS_coreMod, only : LIS_rc
    use LIS_precisionMod
    use clm_varpar,   only : nlevsoi, nlevsno, numrad
! !DESCRIPTION:
!
! Initializes clm variables
!
!  The arguments are:
! \begin{description}
!  \item[n]
!   index of the nest
! \end{description}
!EOP
    integer :: k, n
    do k = 1,LIS_rc%npatch(n,LIS_rc%lsm_index)

!   no more clm(k) now
!       clm45_struc(n)%clm(k)%kpatch  = bigint
!       clm45_struc(n)%clm(k)%itypveg = bigint
!       clm45_struc(n)%clm(k)%itypwat = bigint
!       clm45_struc(n)%clm(k)%isoicol = bigint

   end do 

 end subroutine clm45_lsm_init

!BOP
!
! !ROUTINE: clm45_t2gcr
! \label{clm45_t2gcr}
!
! !INTERFACE:
  subroutine clm45_t2gcr(n, ipe, it, ic, ir, git)
! !USES:
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


