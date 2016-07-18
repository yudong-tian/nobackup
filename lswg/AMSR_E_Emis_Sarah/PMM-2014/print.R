print(" Comparison of Five Prediction Methods", quote=F)
print("==================================================================", quote=F)
print("     | 10V      10H      19V      19H      37V      37H      89V      89H ", quote=F)
print("------------------------------------------------------------------", quote=F)
errs=errs1
print(paste("M1:  |", 
            round(mean(errs[ , , 1]*100), 2),"   ", 
            round(mean(errs[ , , 2]*100), 2),"   ", 
            round(mean(errs[ , , 3]*100), 2),"   ", 
            round(mean(errs[ , , 4]*100), 2),"   ", 
            round(mean(errs[ , , 7]*100), 2),"   ", 
            round(mean(errs[ , , 8]*100), 2),"   ", 
            round(mean(errs[ , , 9]*100), 2),"   ", 
            round(mean(errs[ , , 10]*100), 2) ), quote=F)
errs=errs2
print(paste("M2:  |",
            round(mean(errs[ , , 1]*100), 2),"   ",
            round(mean(errs[ , , 2]*100), 2),"   ",
            round(mean(errs[ , , 3]*100), 2),"   ",
            round(mean(errs[ , , 4]*100), 2),"   ",
            round(mean(errs[ , , 7]*100), 2),"   ",
            round(mean(errs[ , , 8]*100), 2),"   ",
            round(mean(errs[ , , 9]*100), 2),"   ",
            round(mean(errs[ , , 10]*100), 2) ), quote=F)
errs=errs3
print(paste("M3:  |",
            round(mean(errs[ , , 1]*100), 2),"   ",
            round(mean(errs[ , , 2]*100), 2),"   ",
            round(mean(errs[ , , 3]*100), 2),"   ",
            round(mean(errs[ , , 4]*100), 2),"   ",
            round(mean(errs[ , , 7]*100), 2),"   ",
            round(mean(errs[ , , 8]*100), 2),"   ",
            round(mean(errs[ , , 9]*100), 2),"   ",
            round(mean(errs[ , , 10]*100), 2) ), quote=F)
errs=errs4
print(paste("M4:  |",
            round(mean(errs[ , , 1]*100), 2),"   ",
            round(mean(errs[ , , 2]*100), 2),"   ",
            round(mean(errs[ , , 3]*100), 2),"   ",
            round(mean(errs[ , , 4]*100), 2),"   ",
            round(mean(errs[ , , 7]*100), 2),"   ",
            round(mean(errs[ , , 8]*100), 2),"   ",
            round(mean(errs[ , , 9]*100), 2),"   ",
            round(mean(errs[ , , 10]*100), 2) ), quote=F)
errs=errs5
print(paste("M5:  |",
            round(mean(errs[ , , 1]*100), 2),"   ",
            round(mean(errs[ , , 2]*100), 2),"   ",
            round(mean(errs[ , , 3]*100), 2),"   ",
            round(mean(errs[ , , 4]*100), 2),"   ",
            round(mean(errs[ , , 7]*100), 2),"   ",
            round(mean(errs[ , , 8]*100), 2),"   ",
            round(mean(errs[ , , 9]*100), 2),"   ",
            round(mean(errs[ , , 10]*100), 2) ), quote=F)

cat("====================================================================") 


