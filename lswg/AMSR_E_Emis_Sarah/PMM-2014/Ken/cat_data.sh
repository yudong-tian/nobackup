

ddir=/discover/nobackup/kwharris/ytian/

# Ken's data do not include 24G. Use 37G data to fill the position of 24G, 
# just to keep the file layout the same, and 24G is not displayed anyway. 

cat /dev/null > CMEM3_uncal_bias0.10gd4r
cat /dev/null > CMEM3_uncal_bias1.10gd4r
cat /dev/null > CMEM3_cal_bias0.10gd4r
cat /dev/null > CMEM3_cal_bias1.10gd4r

cat /dev/null > CRTM2_uncal_bias0.10gd4r
cat /dev/null > CRTM2_uncal_bias1.10gd4r
cat /dev/null > CRTM2_cal_bias0.10gd4r
cat /dev/null > CRTM2_cal_bias1.10gd4r

for freq in 11 19 37 37 89; do  
 for pol in V H; do  

  cat $ddir/"NoahCMEM3  11   V,11   H-- BIAS_CORRECT0SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CMEM3_cal_bias0.10gd4r
  cat $ddir/"NoahCMEM3  11   V,11   H-- BIAS_CORRECT1SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CMEM3_cal_bias1.10gd4r
  cat $ddir/"NoahCMEM3  uncalibrated-- BIAS_CORRECT0SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CMEM3_uncal_bias0.10gd4r
  cat $ddir/"NoahCMEM3  uncalibrated-- BIAS_CORRECT1SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CMEM3_uncal_bias1.10gd4r

  cat $ddir/"NoahCRTM2EM11   V,11   H-- BIAS_CORRECT0SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CRTM2_cal_bias0.10gd4r
  cat $ddir/"NoahCRTM2EM11   V,11   H-- BIAS_CORRECT1SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CRTM2_cal_bias1.10gd4r
  cat $ddir/"NoahCRTM2EMuncalibrated-- BIAS_CORRECT0SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CRTM2_uncal_bias0.10gd4r
  cat $ddir/"NoahCRTM2EMuncalibrated-- BIAS_CORRECT1SNOWFROZ0RMSE${freq}   ${pol}.bin"  >> CRTM2_uncal_bias1.10gd4r
   
 done
done


