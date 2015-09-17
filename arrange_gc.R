# This function creates table from selected columns of report
# files generated through Agilent GC ChemStation. In order to 
# prepare files for loading, user should put all folders 
# that contain REPORT01.CSV (default name) into one folder.
# Function 'arrange_gc' search for 'REPORT01.CSV' files in each 
# sub-folder, takes values of retention time (RT-FID) and area 
# from it, calculate each peak share in percent (m/m%) and arrange 
# that two columns in new table (final.csv). 
# Arguments:
# directory - path to initial folder
# max - maximal count of peaks. This basically depend on 'Integration events'
# sol - default value is FALSE, but if first peak is peak of solvent user 
# should put 'sol = TRUE'
# new_soft - since Agilent ChemStations produce different CSV files depending 
# software version. User should put 'new_soft = FALSE' 
# if software is older than 2003.

arrange_gc <- function(directory, max = 100, sol = FALSE, new_soft = TRUE) {
  encoding <- c("UTF-16") # set default encoding value
  if(new_soft != TRUE){encoding <- c("UTF-8")} # set encoding value for old software
  fld <- list.dirs(directory) # get list of sub-folders together with parent folder
  fld_count <- length(fld) # get number of sub-folders together with parent folder
  fld <- fld[2:fld_count] # exclude parent folder from a vector
  fld_names <- basename(fld) # create vector of sub-folder names
  final_df <- data.frame(1:max) # create one-column data frame with 'max' number of rows
  count <- length(fld_names) # get count of sub-folders only
  
  for(i in 1:count){
    df <- read.table(file.path(directory, fld_names[i], "REPORT01.csv"), sep =",", 
                     header = FALSE, fileEncoding = encoding) # read REPORT01.CSV from each sub-folder
    if(sol == TRUE){df <- df[-c(1),]} # remove solvent peak area if exists 
    df[, 2] = round(df[,2],3) # set retention time on two decimals
    area_sum <- sum(df[, 5]) # get sum value of area column
    mm <- round(df[, 5] * 100 / area_sum, 2) # calculate peak share in percent, round value on two decimals 
    new_df <- data.frame(df[, 2], mm) # create new data frame with chosen columns - retention time and percentage
    rows_count <- nrow(new_df) # get row number of current data set
    diff <- max - (rows_count + 1) # calculate difference between current data row numbers and predicted 'max' value
    new_df <- rbind(c(fld_names[i], fld_names[i]), new_df) # create header with sub-folder names
    for(i in 1:diff){
      new_df <- rbind(new_df, c(NA, NA)) # fill rest of the rows in data set with NA's
    }
    colnames(new_df) <- c("RT-FID", "%m/m") # set column names
    final_df <- cbind(final_df, new_df, row.names = NULL) # create new data frame for final table
  }
  final_df <- final_df[,-c(1)] # delete initial column of data frame
  write.csv(final_df, file = (file.path(directory, "final.csv"))) # write data frame to file
  print("File 'final.csv' has been created in your initial directory.") # return information to user
}
