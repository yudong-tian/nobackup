
# start/end date
syr=2008
smon=8
sdy=16

eyr=2009
emon=3
edy=31

dstr=${syr}_${smon}-${eyr}_${emon}

#event counts, global
echo 3B42
./duration-intensity $syr $smon $sdy $eyr $emon $edy 3B42 > d-i-$dstr.dat
