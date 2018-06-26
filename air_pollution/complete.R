complete <- function(directory, id = 1:332) {
  # get a list of all data files to load 
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  # create an empty vector to hold the count of non NA data sets
  nobs<-numeric()

  # for each id (device) find the count of complete data sets
  for(i in id){
    data <- read.csv(filelist[i])
    nobs <- c(nobs,sum(complete.cases(data)))
  }
  
  # create a data frame and display
  result <- data.frame(id, nobs)
  
  return(result)
}