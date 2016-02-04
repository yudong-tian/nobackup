
! read from CLM4.5 native parameter file and save it to LDT format
! Usage:
! read_netcdf inputfile outputfile outvarname data_type varname dim1_name dim2_name ... 
! Three data_type: int, float, double 
! Max four dimensions 


  program read_netcdf 

  use netcdf

  implicit none
  include "netcdf.inc"

  character (len = 250) :: infile, outfile 
  character (len = 100) :: data_type, varname, ovarname
  character (200) ::  dim_name(18) 
  integer :: start(4), count(4), dimid(4) ! max 4-dimensional  
  real*8, allocatable  :: ddata4d(:, :, :, : ), odata4d(:, :, :, :) 

  integer :: ncid, varid, iargc, ndims, nargs, i, j, k, l, len, ic 
  integer :: ncid2, varid2, nx, ny, i0, j0

  nargs = iargc() 
  if (nargs .gt. 9 ) then 
   write(*, *)"max dimension is 4! " 
   stop
  end if 

  ndims = nargs - 5 

  call getarg(1, infile) 
  call getarg(2, outfile) 
  call getarg(3, ovarname) 
  call getarg(4, data_type) 
  call getarg(5, varname) 
  do i=1, ndims 
    call getarg(i+5, dim_name(i) ) 
  end do 
  
  call check( nf90_open(infile, NF90_NOWRITE, ncid) )

  !Note: if variable in ncdump output is MONTHLY_SAI(time, lsmpft, lsmlat, lsmlon)
  !  the fortran data array should be data(lsmlon, lsmlat, lsmpft, time),
  !  and so should count array, ie., count=(/lsmlon, lsmlat, lsmpft, time/). 
  ! get all the dimension sizes 
  start(:) = 1
  count(:) = 1
  len = 1 
  do i=ndims, 1, -1  ! swap the dim order for fortran for count() array
    ic = ndims - i + 1  ! reverse order
    call check( nf90_inq_dimid(ncid, trim(dim_name(i)), dimid(i) ) ) 
    call check( nf90_inquire_dimension(ncid, dimid(i), len=count(ic) ) ) 
    !write(*, *) trim(dim_name(i)), " dimid= ", dimid(i), "  dim size=", count(ic) 
    len = len * count(ic) 
  end do 
   write(*, *) (count(i), i=1, ndims) 

  call check( nf90_inq_varid(ncid, trim(varname), varid) )

! forget data type matching. Convert everything to double when reading whatever types
! see https://www.unidata.ucar.edu/software/netcdf/docs/netcdf/Type-Conversion.html#Type-Conversion

  allocate(ddata4d(count(1), count(2), count(3), count(4)))
  call check( nf90_get_var(ncid, varid, ddata4d, start=start, count=count ) )
  ! Close the file, freeing all resources.
  call check( nf90_close(ncid) )

! special treatment for "lon_b" and "lat_b" variables 
    if (trim(ovarname) .eq. 'lon_b' .or. trim(ovarname) .eq. 'lat_b') then 
      allocate(odata4d(count(1)+4, count(2)+4, count(3), count(4)))
      nx = count(1) 
      ny = count(2) 
      Do j=-1, ny+2
        j0 = min(max(j, 1), ny) 
        Do i=-1, nx+2
          i0 = min(max(i, 1), nx) 
          odata4d(i+2, j+2, :, :) = ddata4d(i0, j0, :, :) 
        End Do 
      End Do 
    else 
      allocate(odata4d(count(1), count(2), count(3), count(4)))
      odata4d = ddata4d
    end if 
      

! save to output file 

     call check( nf90_open(outfile, NF90_WRITE, ncid2) )
     call check( nf90_inq_varid(ncid2, trim(ovarname), varid2) )
     call check( nf90_put_var(ncid2, varid2, odata4d ) )
     call check( nf90_close(ncid2) )


  deallocate(ddata4d)  
  deallocate(odata4d)  


contains

  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  

end program read_netcdf 

