cc -c fmktime.c
f90 -YEXT_NAMES=LCS -B108 -c subroutines-trmm.F90 
f90 -YEXT_NAMES=LCS -B108 -o duration-intensity duration-intensity.F90 fmktime.o subroutines-trmm.o -lU77
