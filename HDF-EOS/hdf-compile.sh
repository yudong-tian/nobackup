
export INC_HDF=/discover/nobackup/mbhat/Soni/HDF4.2r4/hdf4/include
export LIB_HDF=/discover/nobackup/mbhat/Soni/HDF4.2r4/hdf4/lib
export INC_HDFEOS=/discover/nobackup/mbhat/Soni/hdfeos/include
export LIB_HDFEOS=/discover/nobackup/mbhat/Soni/hdfeos/lib

mpif90 -g -u -names lowercase -convert big_endian -assume byterecl \
 -I$INC_HDF -L$LIB_HDF -ljpeg -lz -lm -o hdf-only hdf-only.F90 \
 $LIB_HDF/libmfhdf.a $LIB_HDF/libdf.a $LIB_HDFEOS/libsz.a





