corr <- function(directory, threshold = 0) {
  # get a list of all data files to load 
  filelist <- list.files(directory, full.names = TRUE)
  
  # get the device (id) wise count of complete data set
  nobs_table <- complete("specdata", 1:332)
  
  # list of the devices (id) where the count of complete dataset is greater than threshold
  ids <- nobs_table$id[nobs_table$nobs > threshold]
  corr_res <- numeric()
  
  # calculate the correlation between sulfate and nitrate for complete data sets
  for(i in ids){
    data <- read.csv(filelist[i])
    corr_res <- c(corr_res,cor(data$sulfate, data$nitrate, use="complete.obs"))
  }
  return(corr_res)
}