
export INC_HDF=/discover/nobackup/mbhat/Soni/HDF4.2r4/hdf4/include
export LIB_HDF=/discover/nobackup/mbhat/Soni/HDF4.2r4/hdf4/lib
export INC_HDFEOS=/discover/nobackup/mbhat/Soni/hdfeos/include
export LIB_HDFEOS=/discover/nobackup/mbhat/Soni/hdfeos/lib

ifort -g -u -names lowercase  -nomixed_str_len_arg -convert big_endian -assume byterecl \
 -I$INC_HDF -I$INC_HDFEOS \
-o read-geos5-fp read-geos5-fp.F90 \
$LIB_HDFEOS/libhdfeos.a $LIB_HDFEOS/libGctp.a $LIB_HDF/libmfhdf.a \
$LIB_HDF/libdf.a \
 -ljpeg -lz -lm \
$LIB_HDFEOS/libsz.a



