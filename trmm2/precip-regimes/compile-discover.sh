cc -c fmktime.c
ifort -convert big_endian -assume byterecl -c subroutines.F90 
ifort -convert big_endian -assume byterecl -o duration-intensity duration-intensity.F90  fmktime.o subroutines.o 
