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
    use clmtypeInitMod  
    use clmtype
    use clm_time_manager , only : get_nstep, get_step_size, set_timemgr_init, timemgr_init, &
                                  get_curr_calday, get_curr_date
    use clm_varpar      , only : maxpatch, clm_varpar_init, more_vertlayers
    use clm_varcon      , only : clm_varcon_init
    use clm_varctl      , only : fsurdat, fatmlndfrc, flndtopo, fglcmask, noland, &
                                 create_glacier_mec_landunit, allocate_all_vegpfts, &
				 create_crop_landunit, single_column, nsrStartup, finidat, &
                                 co2_type, co2_ppmv, fpftcon, fsnowaging, fsnowoptics, nsegspc 
    use pftvarcon       , only : clm45_pftconrd
    use surfrdMod
    use clm_atmlnd   ,   only : atm2lnd_type, clm_a2l, clm_l2a,  clm_map2gcell, init_atm2lnd_type, &
                                init_lnd2atm_type
    use decompInitMod   , only : decompInit_lnd, decompInit_glcp
    use clm45_decompMod       
    use domainMod       , only : domain_check, ldomain, domain_init
    use spmdMod         , only : masterproc, mpicom
    use UrbanInputMod
    use DUSTMod
    use initGridCellsMod, only : initGridCells
    use perf_mod
    use initSurfAlbMod
    use shr_orb_mod
    use STATICEcosysDynMod
    use UrbanInitMod 
    use UrbanMod 
!YDT    use accFldsMod
    use filterMod 
    use reweightMod 

  ! LIS modules
  use clm45_module
  use LIS_logMod
  use LIS_mpiMod,  only : LIS_mpi_comm 

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
   use mkarbinitMod
   use initSLakeMod
   use accFldsMod
   use SurfaceAlbedoMod, only: albice
! !DESCRIPTION:        
!
!EOP
   implicit none
   integer :: n, i, t, g, gic, gir, gid, git, nc, nsrest, lnclumps
   integer                 :: yr, mo, da, hr, mn, ss
   integer                 :: mon, day, ncsec, dtime
   integer                 :: symd, eymd, stod, etod  ! clm date format: yyyymmdd as integer 
   real(r8)	           ::  eccen, mvelpp, lambm0, obliqr, declin, eccf, ceclinm1, calday
   real(r8)	           ::  declinm1, caldaym1 
   integer                 :: status
   logical :: arbinit
   character*3   :: fnest

   integer :: begg, endg, begl, endl, begc, endc, begp, endp
   integer :: ni, nj, ns, ic, ip, il, ig
   integer :: ier,  tmpid

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
   ! could be moved to lis config file 

   nsrest = 0   ! cold start 
   if( LIS_rc%startcode.eq."restart" ) nsrest = 1

     create_glacier_mec_landunit = .false.
     allocate_all_vegpfts = .true.  
     single_column = .false.
     ! used to be in decompMod. number of clumps per cpu
     clump_pproc = 1

   ! these were in lnd_in run-time config
    albice(1) = 0.60
    albice(2) = 0.40
    co2_ppmv = 284.7
    co2_type = 'constant'
    create_crop_landunit = .false.
    !YDT dtime = 1800
    fatmlndfrc = '/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/share/domains/domain.lnd.fv0.9x1.25_gx1v6.090309.nc'
    finidat = '/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/lnd/clm2/initdata_map/clmi.I1850CRUCLM45SP.0521-01-01.0.9x1.25_g1v6_simyr1850_c130506.nc'
    fpftcon = '/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/lnd/clm2/pftdata/pft-physiology.c130503.nc'
    fsnowaging = '/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/lnd/clm2/snicardata/snicar_drdt_bst_fit_60_c070416.nc'
    fsnowoptics = '/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/lnd/clm2/snicardata/snicar_optics_5bnd_c090915.nc'
    fsurdat = '/discover/nobackup/projects/nca/ytian/CESM_INPUT_DATA/lnd/clm2/surfdata_map/surfdata_0.9x1.25_simyr1850_c130415.nc'
    maxpatch_glcmec = 0
    more_vertlayers = .false.
    nsegspc = 20
    urban_hac = 'ON'
    urban_traffic = .false.

     ! let clm know LIS's mpi world
     mpicom = LIS_mpi_comm
     call spmd_init(mpicom, 1) 

    symd =  LIS_rc%syr*10000 + LIS_rc%smo * 100 + LIS_rc%sda
    stod =  (LIS_rc%shr*60+LIS_rc%smn)*60+LIS_rc%sss      ! seconds of the day 
    eymd =  LIS_rc%eyr*10000 + LIS_rc%emo * 100 + LIS_rc%eda
    etod =  (LIS_rc%ehr*60+LIS_rc%emn)*60+LIS_rc%ess      ! seconds of the day 

    ! Connect CLM time manager to LIS's 
    call set_timemgr_init( calendar_in='NO_LEAP', start_ymd_in=symd, start_tod_in=stod, &
                           ref_ymd_in=symd, ref_tod_in=stod, stop_ymd_in=eymd,         &
                           stop_tod_in=etod,  perpetual_run_in=.false.,     &
                           perpetual_ymd_in=-999, dtime_in=nint(LIS_rc%ts) )

    call timemgr_init()


   do n = 1, LIS_rc%nnest

     ! Read list of PFTs and their corresponding parameter values
     ! Independent of model resolution, Needs to stay before surfrd_get_data

     call clm45_pftconrd(clm45_struc(n)%clm_vfile)  

     ! need to make nest-dependent (but they are  tile-independent variables and structures ) 
      call clm_varpar_init()   ! number of patches, vertical levels, etc
      call clm_varcon_init()   ! physical constants and thickness of vertical levels 

      ! translate LIS decomposed domain into CLM45 domain
      write(LIS_logunit,*) 'clm_varpar_init and clm_varcon_init done. '  

     ni = LIS_ewe_halo_ind(n,LIS_localPet+1) - LIS_ews_halo_ind(n,LIS_localPet+1) + 1 
     nj = LIS_nse_halo_ind(n,LIS_localPet+1) - LIS_nss_halo_ind(n,LIS_localPet+1) + 1 
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
     call domain_check(ldomain)

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

     call initClmtype()

     call init_atm2lnd_type(begg, endg, clm_a2l)
     call init_lnd2atm_type(begg, endg, clm_l2a)  ! should not be needed 

     call initGridcells()

! YDT check weights 
!     call get_proc_bounds (begg, endg, begl, endl, begc, endc, begp, endp)
!     Do ic = begc, endc 
!       write(LIS_logunit, '(A, I7, A, F7.4)' ) 'col=', ic, ' col%wtgcell(ic)=', col%wtgcell(ic) 
!     End Do 

     deallocate (vegxy, wtxy, topoxy)

!YDT ------------- end of initialize1() --------------------------------

!YDT ------------- start of initialize2() --------------------------------

      call EcosystemDynini()

      call Dustini()
!YDT    call VOCEmission_init( )

      call UrbanInitTimeConst()
      call iniTimeConst(n)             ! nest passed

!YDT: cold start initialization. To be replaced from restart file reading 
       call mkarbinit()

       call UrbanInitTimeVar( )

       arbinit = .true.
       call initSLake(arbinit)

     call initAccFlds()

     call initAccClmtype()

     call allocFilters()

!YDT reweight will call setFilters() 
    lnclumps = get_proc_clumps()
    write(LIS_logunit,*) 'lnclumps =', lnclumps 
    do nc = 1, lnclumps
       call reweightWrapup(nc)
    end do

    call UrbanInitAero()
    ! Initialize urban radiation model - this uses urbinp data structure

    call UrbanClumpInit()

    ! Finalize urban model initialization
    call clm45_UrbanInput(n, mode='finalize')

    ! End initialization

    if (masterproc) then
       write(LIS_logunit,*) 'Successfully initialized the land model'
       if (nsrest == nsrStartup) then
          write(LIS_logunit,*) 'begin initial run at: '
       else
          write(LIS_logunit,*) 'begin continuation run at:'
       end if
       call get_curr_date(yr, mon, day, ncsec)
       write(LIS_logunit,*) '   nstep= ',get_nstep(), ' year= ',yr,' month= ',mon,&
            ' day= ',day,' seconds= ',ncsec
       write(LIS_logunit,*)
       write(LIS_logunit,'(72a1)') ("*",i=1,60)
       write(LIS_logunit,*)
    endif

    if (get_nstep() == 0 .or. nsrest == nsrStartup) then
       ! Initialize albedos (correct pft filters are needed)

!YDT 
       do_initsurfalb = .true.
       if (finidat == ' ' .or. do_initsurfalb) then
          call t_startf('init_orb')
          calday = get_curr_calday()
          call t_startf('init_orbd1')
          call shr_orb_decl( calday, eccen, mvelpp, lambm0, obliqr, declin, eccf )
          call t_stopf('init_orbd1')

          dtime = get_step_size()
          caldaym1 = get_curr_calday(offset=-int(dtime))
          call t_startf('init_orbd2')
          call shr_orb_decl( caldaym1, eccen, mvelpp, lambm0, obliqr, declinm1, eccf )
          call t_stopf('init_orbd2')

          call t_startf('init_orbSA')
          call initSurfalb( calday, declin, declinm1 )
          call t_stopf('init_orbSA')
          call t_stopf('init_orb')
       !YDT
       !YDT else if ( n_drydep > 0 .and. drydep_method == DD_XLND )then
          ! Call interpMonthlyVeg for dry-deposition so that mlaidiff will be calculated
          ! This needs to be done even if CN or CNDV is on!
       !YDT    call interpMonthlyVeg()
       end if

       ! Determine gridcell averaged properties to send to atm
       call print_glcp('p')  !debugging print

!       write(LIS_logunit, *) pps%albd

       call t_startf('init_map2gc')
       call clm_map2gcell(init=.true.)
       call t_stopf('init_map2gc')
    end if 


!YDT ------------- end of initialize2() --------------------------------

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
    use LIS_LMLCMod
 
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
    End Do 

    cid = LIS_localPet + 1
    procinfo%cid(clump_pproc)  = cid 

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
    write(LIS_logunit,*) 'Domain to be initialed with beg=', beg, ' end=', end 
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
                if (LIS_LMLC(n)%landmask(ai,aj) .gt. 0 ) then 
                  ag = ag + 1
                  ldecomp%gdc2glo(ag) = an
                  ldecomp%glo2gdc(an) = ag
                end if 
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


