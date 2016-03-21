
export INC_HDF=/home/ytian/proj-disk/libs/HDF4.2r4/include
export LIB_HDF=/home/ytian/proj-disk/libs/HDF4.2r4/lib
#export INC_HDF=/discover/nobackup/mbhat/Soni/HDF4.2r4/hdf4/include
#export LIB_HDF=/discover/nobackup/mbhat/Soni/HDF4.2r4/hdf4/lib
#export INC_HDFEOS=/discover/nobackup/mbhat/Soni/hdfeos/include
#export LIB_HDFEOS=/discover/nobackup/mbhat/Soni/hdfeos/lib

cc -c fmktime.c

ifort -g -u -debug extended -names lowercase -convert big_endian -assume byterecl \
 -I$INC_HDF -L$LIB_HDF -ljpeg -lz -lm -o read_AE_hdf read_AE_hdf.F90 \
 $LIB_HDF/libmfhdf.a $LIB_HDF/libdf.a fmktime.o

#$LIB_HDFEOS/libsz.a





