pollutantmean <- function(directory, pollutant, id = 1:332) {
  # get a list of all data files to load 
  allfiles <- list.files(directory, full.names = TRUE)
  
  # load all data files into a single data.frame for analysis
  all_data <- do.call(rbind,lapply(allfiles,read.csv))
  
  # fetch a subset of data to analyze w.r.t to the input  
  result <- subset(all_data, ID >= range(id)[1] & ID <= range(id)[2], select = pollutant )

  # find the mean of the range for the pollutant leaving NAs
  result <- mean(na.omit(result)[[1]])
  
  return(result)
}