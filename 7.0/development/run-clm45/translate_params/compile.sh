#! /bin/bash 


ifort  -c -free -check -traceback -I.  \
  -I/usr/local/other/SLES11.1/netcdf/4.1.3/intel-12.1.0.233/include \
  -O2 -fp-model precise -convert big_endian -assume byterecl -ftz -traceback -DMAXPATCH_PFT=17 -DLINUX  \
  -DNDEBUG -DMCT_INTERFACE -DHAVE_MPI -DFORTRANUNDERSCORE -DNO_R16 -DFORTRANUNDERSCORE -DNO_R16 \
  -DHAVE_F2008_CONTIGUOUS -DLINUX -DCPRINTEL  -DHAVE_SLASHPROC -free \
  translate_to_LDT.F90  do_swap_lon.F90

ifort  -free -check -traceback -o translate_to_LDT translate_to_LDT.o \
  -L/usr/local/other/SLES11.1/netcdf/4.1.3/intel-12.1.0.233/lib \
  -lnetcdff -lnetcdf -lm -lcurl  

ifort  -free -check -traceback -o do_swap_lon do_swap_lon.o \
  -L/usr/local/other/SLES11.1/netcdf/4.1.3/intel-12.1.0.233/lib \
  -lnetcdff -lnetcdf -lm -lcurl  


