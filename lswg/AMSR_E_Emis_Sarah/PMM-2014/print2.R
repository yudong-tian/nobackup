cat("\n  Table X: Comparison of Five Prediction Methods\n") 
cat("====================================================================\n") 
cat("     |  10V     10H     19V     19H     37V     37H     89V     89H\n")
cat("--------------------------------------------------------------------\n") 
errs=errs1
cat(paste("M1:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs2
cat(paste("M2:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs3
cat(paste("M3:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs4
cat(paste("M4:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 
errs=errs5
cat(paste("M5:  |", 
            round(mean(errs[ , , 1]*100), 2),
            round(mean(errs[ , , 2]*100), 2),
            round(mean(errs[ , , 3]*100), 2),
            round(mean(errs[ , , 4]*100), 2),
            round(mean(errs[ , , 7]*100), 2),
            round(mean(errs[ , , 8]*100), 2),
            round(mean(errs[ , , 9]*100), 2),
            round(mean(errs[ , , 10]*100), 2), "\n", sep='\t' ) ) 

cat("====================================================================\n") 


