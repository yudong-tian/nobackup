#include "LIS_misc.h"

module clm45_decompMod

!------------------------------------------------------------------------------
!BOP
!
! !MODULE: decompMod
!
! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use spmdMod     , only : masterproc, iam, npes, mpicom, comp_id
!  use mct_mod
  use abortutils  , only : endrun

  use LIS_logMod,       only : LIS_verify, LIS_logunit
!
! !PUBLIC TYPES:
  implicit none
  integer, public :: clump_pproc ! number of clumps per MPI process
!
! !PUBLIC MEMBER FUNCTIONS:
  public get_clump_bounds   ! clump beg and end gridcell,landunit,column,pft
  public get_proc_clumps    ! number of clumps for this processor
  public get_proc_bounds    ! this processor beg and end gridcell,landunit,column,pft
  public get_proc_total     ! total number of gridcells, landunits,
                            ! columns and pfts for any processor
  public get_proc_global    ! total gridcells, landunits, columns, pfts
                            ! across all processors
  public get_clmlevel_gsize ! get global size associated with clmlevel
!YDT  public get_clmlevel_gsmap ! get gsmap associated with clmlevel
!
! !DESCRIPTION:
! Module provides a descomposition into a clumped data structure which can
! be mapped back to atmosphere physics chunks.
!
! !REVISION HISTORY:
! 2002.09.11  Forrest Hoffman  Creation.
! 2005.11.01  T Craig  Rewrite
! 2006.06.06  T Craig  Reduce memory, cleanup
!
!
! !PRIVATE TYPES:
  private  ! (now mostly public for decompinitmod)

  integer,public :: nclumps     ! total number of clumps across all processors
  integer,public :: numg        ! total number of gridcells on all procs
  integer,public :: numl        ! total number of landunits on all procs
  integer,public :: numc        ! total number of columns on all procs
  integer,public :: nump        ! total number of pfts on all procs

  !---global information on each pe
  type processor_type
     integer :: nclumps          ! number of clumps for processor_type iam
     integer,pointer :: cid(:)   ! clump indices
     integer :: ncells           ! number of gridcells in proc
     integer :: nlunits          ! number of landunits in proc
     integer :: ncols            ! number of columns in proc
     integer :: npfts            ! number of pfts in proc
     integer :: begg, endg       ! beginning and ending gridcell index
     integer :: begl, endl       ! beginning and ending landunit index
     integer :: begc, endc       ! beginning and ending column index
     integer :: begp, endp       ! beginning and ending pft index
  end type processor_type
  public processor_type
  type(processor_type),public :: procinfo

  !---global information on each pe
  type clump_type
     integer :: owner            ! process id owning clump
     integer :: ncells           ! number of gridcells in clump
     integer :: nlunits          ! number of landunits in clump
     integer :: ncols            ! number of columns in clump
     integer :: npfts            ! number of pfts in clump
     integer :: begg, endg       ! beginning and ending gridcell index
     integer :: begl, endl       ! beginning and ending landunit index
     integer :: begc, endc       ! beginning and ending column index
     integer :: begp, endp       ! beginning and ending pft index
  end type clump_type
  public clump_type
  type(clump_type),public, allocatable :: clumps(:)

  !---global information on each pe
  !--- i,j = 2d global
  !--- glo = 1d global sn ordered
  !--- gsn = 1d global sn ordered compressed
  !--- gdc = 1d global dc ordered compressed
  type decomp_type
     integer,pointer :: glo2gdc(:)    ! 1d glo to 1d gdc
     integer,pointer :: gdc2glo(:)    ! 1d gdc to 1d glo
  end type decomp_type
  public decomp_type
  type(decomp_type),public,target :: ldecomp

!YDT  type(mct_gsMap)  ,public,target :: gsMap_lnd_gdc2glo
!YDT  type(mct_gsMap)  ,public,target :: gsMap_gce_gdc2glo
!YDT  type(mct_gsMap)  ,public,target :: gsMap_lun_gdc2glo
!YDT  type(mct_gsMap)  ,public,target :: gsMap_col_gdc2glo
!YDT  type(mct_gsMap)  ,public,target :: gsMap_pft_gdc2glo

!EOP
!------------------------------------------------------------------------------
! 
! 
!------------------------------------------------------------------------------

contains

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_clump_bounds
!
! !INTERFACE:
   subroutine get_clump_bounds (n, begg, endg, begl, endl, begc, endc, begp, endp)
!
! !USES:
!
! !ARGUMENTS:
     implicit none
     integer, intent(in)  :: n           ! proc clump index
     integer, intent(out) :: begp, endp  ! clump beg and end pft indices
     integer, intent(out) :: begc, endc  ! clump beg and end column indices
     integer, intent(out) :: begl, endl  ! clump beg and end landunit indices
     integer, intent(out) :: begg, endg  ! clump beg and end gridcell indices
!
! !DESCRIPTION:
! Determine clump beginning and ending pft, column, landunit and gridcell indices.
!
! !REVISION HISTORY:
! 2003.09.12  Mariana Vertenstein  Creation.
!
! !LOCAL VARIABLES:
!EOP
     character(len=32), parameter :: subname = 'get_clump_bounds'  ! Subroutine name
     integer :: cid                                                ! clump id
#ifdef _OPENMP
     integer, external :: OMP_GET_MAX_THREADS
     integer, external :: OMP_GET_NUM_THREADS
#endif
!------------------------------------------------------------------------------
!
!    Make sure this IS being called from a threaded region
!
#ifdef _OPENMP
     if ( OMP_GET_NUM_THREADS() == 1 .and. OMP_GET_MAX_THREADS() > 1 )then
        call endrun( trim(subname)//' ERROR: Calling from inside a non-threaded region -- this results in bad performance' )
     end if
#endif

     cid  = procinfo%cid(n)
     begp = clumps(cid)%begp
     endp = clumps(cid)%endp
     begc = clumps(cid)%begc
     endc = clumps(cid)%endc
     begl = clumps(cid)%begl
     endl = clumps(cid)%endl
     begg = clumps(cid)%begg
     endg = clumps(cid)%endg

   end subroutine get_clump_bounds

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_proc_bounds
!
! !INTERFACE:
   subroutine get_proc_bounds (begg, endg, begl, endl, begc, endc, begp, endp)
!
! !USES:
!
! !ARGUMENTS:
     implicit none
     integer, optional, intent(out) :: begp, endp  ! proc beg and end pft indices
     integer, optional, intent(out) :: begc, endc  ! proc beg and end column indices
     integer, optional, intent(out) :: begl, endl  ! proc beg and end landunit indices
     integer, optional, intent(out) :: begg, endg  ! proc beg and end gridcell indices
! !DESCRIPTION:
! Retrieve gridcell, landunit, column, and pft bounds for process.
!
! !REVISION HISTORY:
! 2003.09.12  Mariana Vertenstein  Creation.
!
! !LOCAL VARIABLES:
!EOP
     character(len=32), parameter :: subname = 'get_proc_bounds'  ! Subroutine name
#ifdef _OPENMP
     integer, external :: OMP_GET_NUM_THREADS
#endif
!------------------------------------------------------------------------------
!
!    Make sure this is NOT being called from a threaded region
!
#ifdef _OPENMP
     if ( OMP_GET_NUM_THREADS() > 1 )then
        call endrun( trim(subname)//' ERROR: Calling from inside a threaded region -- this is illegal' )
     end if
#endif

     if (present(begp)) begp = procinfo%begp
     if (present(endp)) endp = procinfo%endp
     if (present(begc)) begc = procinfo%begc
     if (present(endc)) endc = procinfo%endc
     if (present(begl)) begl = procinfo%begl
     if (present(endl)) endl = procinfo%endl
     if (present(begg)) begg = procinfo%begg
     if (present(endg)) endg = procinfo%endg

   end subroutine get_proc_bounds

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_proc_total
!
! !INTERFACE:
   subroutine get_proc_total(pid, ncells, nlunits, ncols, npfts)
!
! !DESCRIPTION:
! Count up gridcells, landunits, columns, and pfts on process.
!
! !USES:
!
! !ARGUMENTS:
     implicit none
     integer, intent(in)  :: pid     ! proc id
     integer, intent(out) :: ncells  ! total number of gridcells
                                     ! on the processor
     integer, intent(out) :: nlunits ! total number of landunits
                                     ! on the processor
     integer, intent(out) :: ncols   ! total number of columns
                                     ! on the processor
     integer, intent(out) :: npfts   ! total number of pfts
                                     ! on the processor
!
! !REVISION HISTORY:
! 2003.09.12  Mariana Vertenstein  Creation.
!
!
! !LOCAL VARIABLES:
!EOP
   integer :: cid       ! clump index
!------------------------------------------------------------------------------

     npfts   = 0
     nlunits = 0
     ncols   = 0
     ncells  = 0
     do cid = 1,nclumps
        if (clumps(cid)%owner == pid) then
           ncells  = ncells  + clumps(cid)%ncells
           nlunits = nlunits + clumps(cid)%nlunits
           ncols   = ncols   + clumps(cid)%ncols
           npfts   = npfts   + clumps(cid)%npfts
        end if
     end do

   end subroutine get_proc_total

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_proc_global
!
! !INTERFACE:
   subroutine get_proc_global(ng, nl, nc, np)
!
! !DESCRIPTION:
! Return number of gridcells, landunits, columns, and pfts across all
! processes.
!
! !USES:
!
! !ARGUMENTS:
     implicit none
     integer, intent(out) :: ng  ! total number of gridcells across all processors
     integer, intent(out) :: nl  ! total number of landunits across all processors
     integer, intent(out) :: nc  ! total number of columns across all processors
     integer, intent(out) :: np  ! total number of pfts across all processors
! !REVISION HISTORY:
! 2003.09.12  Mariana Vertenstein  Creation.
!
!EOP
!------------------------------------------------------------------------------

     np = nump
     nc = numc
     nl = numl
     ng = numg

   end subroutine get_proc_global

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_proc_clumps
!
! !INTERFACE:
   integer function get_proc_clumps()
!
! !DESCRIPTION:
! Return the number of clumps.
!
! !USES:
!
! !ARGUMENTS:
     implicit none
!
! !RETURN VALUE:
!    integer :: get_proc_clumps
!
! !REVISION HISTORY:
! 2003.09.12  Mariana Vertenstein  Creation.
!
!EOP
!------------------------------------------------------------------------------

     get_proc_clumps = procinfo%nclumps

   end function get_proc_clumps

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_clmlevel_gsize
!
! !INTERFACE:
  integer function get_clmlevel_gsize (clmlevel)
!
! !DESCRIPTION:
! Determine 1d size from clmlevel
!
! !USES:
  use clmtype  , only : grlnd, nameg, namel, namec, namep
  use domainMod, only : ldomain
!
! !ARGUMENTS:
    implicit none
    character(len=*), intent(in) :: clmlevel    !type of clm 1d array
!
! !REVISION HISTORY:
!
!
! !LOCAL VARIABLES:
!EOP

!-----------------------------------------------------------------------

    select case (clmlevel)
    case(grlnd)
       get_clmlevel_gsize = ldomain%ns
    case(nameg)
       get_clmlevel_gsize = numg
    case(namel)
       get_clmlevel_gsize = numl
    case(namec)
       get_clmlevel_gsize = numc
    case(namep)
       get_clmlevel_gsize = nump
    case default
       write(LIS_logunit,*) 'get_clmlevel_gsize does not match clmlevel type: ', trim(clmlevel)
       call endrun()
    end select

  end function get_clmlevel_gsize


!------------------------------------------------------------------------------

end module clm45_decompMod
