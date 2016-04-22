




2/3/2016
4/22/2016

Utilities to transform CLM4.5 domain and surfdata to LDT format. 

create_cdl.sh: creat cdl and empty nc file,  ldt_clm45.nc. 
fill_ldt.sh:  fill it with domain and surface parameters, everyting but the following 
		two fields:

SURFACETYPE: a 0/1 mask of (i, j, vtype)
LANDCOVER:   a weight of (i, j, vtype)   

These two fields were filled by intercepting a live CESM run for now: 
  
/home/ytian/CESM/LIS-CLM4.5SP/pio-xlis-bld/single_col.F90

with the following structure: 


    !max_pft_per_gcell = numpft+1 + 3 + maxpatch_urb*numurbl + maxpatch_glcmec
    ! numpft  maxpatch_urb numurbl  maxpatch_glcmec  numcft max_pft_per_gcell
    ! ---------------------------------------------------------------------------
    !    16         5         3         0         2        35
    !YDT  intercepting pft data and save to (overwrite) LDT file, run with single  CPU
    ! clm pft 0-16: map to landcover and surfacetype(:, :, z), z=1-17
    !     pft 61-65 (urban):  z: 18-22, 23-27, 28-32; (max 3 urban landunits )
    !     pft 61-65 (urban):  z: 18-22, 23-27, 28-32; (max 3 urban landunits )
    !     deep lake:  3 (itype_lun)  z: 33
    !     wetland:  5 (itype_lun)  z: 34
    !     glacier: 2 (itype_lun)   z: 35


Here is the snippet: 

    ldt_file = "/home/ytian/7.0-development/run-clm45/translate_params/ldt_clm45.nc"
    surfacetype = 0
    landcover = 0.0
    do g=begg, endg
       ic = mod(grc%gindex(g), ncol)
       if (ic .eq. 0 ) ic = 288
       ir = ( grc%gindex(g) - ic  )/ncol + 1
       write(*, *) "g=", g, " ic=", ic, " ir=", ir
       do p=grc%pfti(g), grc%pftf(g)
         ptype = pft%itype(p)
         ctype = col%itype(pft%column(p))
         ltype = lun%itype(pft%landunit(p))
         if ( ptype .ge. 0 .and. ptype .le. 16 .and. ltype .eq. 1 )  then
            iz = pft%itype(p) + 1
            surfacetype(ic, ir, iz) = 1
            landcover(ic, ir, iz) = pft%wtgcell(p)
         end if
         ! logic to put urban in: try three times for the possible max 3 land units.
         ! since there are no ptype flags to differeniate them.
         If ( ctype .ge. 61 .and. ctype .le. 65 .and. ltype .eq. 6 )  then
             iz = ctype - 43
             if (surfacetype(ic, ir, iz) .eq. 0 ) then
                  surfacetype(ic, ir, iz) = 1
                  landcover(ic, ir, iz) = pft%wtgcell(p)
             else
                  iz = ctype - 43 + 5
                  if (surfacetype(ic, ir, iz) .eq. 0 ) then
                      surfacetype(ic, ir, iz) = 1
                      landcover(ic, ir, iz) = pft%wtgcell(p)
                  else
                    iz = ctype - 43 + 10
                    if (surfacetype(ic, ir, iz) .eq. 0 ) then
                       surfacetype(ic, ir, iz) = 1
                       landcover(ic, ir, iz) = pft%wtgcell(p)
                    else
                      write(*, *) "Can not find a slot for urban pft, p=", p
                    end if
                 end if
              end if
          End if


          If ( ltype .eq. 2 )  then   ! glacier
            iz = 35
            surfacetype(ic, ir, iz) = 1
            landcover(ic, ir, iz) = pft%wtgcell(p)
          End if

          If ( ltype .eq. 3 )  then   ! deep lake
            iz = 33
            surfacetype(ic, ir, iz) = 1
            landcover(ic, ir, iz) = pft%wtgcell(p)
          End if

          If ( ltype .eq. 5 )  then   ! wet land
            iz = 34
            surfacetype(ic, ir, iz) = 1
            landcover(ic, ir, iz) = pft%wtgcell(p)
          End if


           !  write(*, '(2I7, F10.2, I7, F10.2, I7, F10.2, 3I5)') p, pft%column(p), pft%wtcol(p)*100, pft%landunit(p), &
            !                            pft%wtlunit(p)*100, &
            !                            pft%gridcell(p), pft%wtgcell(p)*100, pft%itype(p), &
            !                            col%itype(pft%column(p)), lun%itype(pft%landunit(p))
        end do   ! p loop
     end do  ! g loop

     call check( nf90_open(ldt_file, NF90_WRITE, ncid2) )
     call check( nf90_inq_varid(ncid2, "SURFACETYPE", varid2) )
     call check( nf90_put_var(ncid2, varid2, surfacetype ) )
     call check( nf90_inq_varid(ncid2, "LANDCOVER", varid2) )
     call check( nf90_put_var(ncid2, varid2, landcover ) )
     call check( nf90_close(ncid2) )



