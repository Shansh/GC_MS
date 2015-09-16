# This function creates table from selected columns of report
# files generated through Agilent GC ChemStation. In order to 
# prepare files for loading, user should put all folders 
# that contain REPORT01.CSV (default name) into one folder.
# Function 'arrange_gc' search for 'REPORT01.CSV' files in each 
# subfolder, takes values of retention time (RT-FID) and area 
# from it, calculate each peak share in percents (m/m%) and arrange 
# that two columns in new table (final.csv). 
# Arguments:
# directory - path to initial folder
# max - maximal count of peaks. This basicaly depend on 'Integration events'
# sol - default value is FALSE, but if first peak is peak of solvent user 
# should put 'sol = TRUE'
# new_soft - since Agilent ChemStations produce different CSV files depending 
# software version. User should put 'new_soft = FALSE' 
# if software is older than 2003.

arrange_gc <- function(directory, max = 100, sol = FALSE, new_soft = TRUE) {
        encoding <- c("UTF-16")
        fld <- list.dirs(directory)
        fld_count <- length(fld)
        fld <- fld[2:fld_count]
        fld_names <- basename(fld)
        final_df <- data.frame(1:max)
        count <- length(fld_names)
  
for(i in 1:count){
        if(new_soft != TRUE){encoding <- c("UTF-8")}
    df <- read.table(file.path(directory, fld_names[i], "REPORT01.csv"), sep =",", header = FALSE, fileEncoding = encoding)
        if(sol == TRUE){df <- df[-c(1),]}    
    df[, 2] = round(df[,2],3)
    area_sum <- sum(df[, 5])
    mm <- round(df[, 5] * 100 / area_sum, 2)
    new_df <- data.frame(df[, 2], mm)
    rows_count <- nrow(new_df)
    diff <- max - (rows_count + 1)
    new_df <- rbind(c(fld_names[i], fld_names[i]), new_df)
          for(i in 1:diff){
              new_df <- rbind(new_df, c(NA, NA))
          }
    colnames(new_df) <- c("RT-FID", "%m/m")
    final_df <- cbind(final_df, new_df, row.names = NULL)
  }
final_df <- final_df[,-c(1)]
write.csv(final_df, file = "final.csv")
print("File 'final.csv' has been created in your working directory.")
}

