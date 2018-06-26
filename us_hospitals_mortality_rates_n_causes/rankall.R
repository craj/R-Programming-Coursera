rankall <- function(outcome, num = "best") {

    # load the data set into hosp_data
  hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # list of unique and valid states and sorted in asc order
  valid_states <- sort(unique(hosp_data$State))

  # list of valid outcomes
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  } else {
    
    # identify the column no. to perform sorting/ranking on
    if(outcome == "heart attack") {
      outcome_col <- 11
    } else if(outcome == "heart failure") {
      outcome_col <- 17
    } else {
      outcome_col <- 23
    }
    
    # create an empty list to store the statewise results
    resultlist = list()
    
    
    # for each 'state' find the requested ranking hospital for the outcome
    for(i in 1:length(valid_states)) {
      state_hosps <- hosp_data[hosp_data$State == valid_states[i],]
      state_hosps[,outcome_col] <- suppressWarnings(as.numeric(state_hosps[,outcome_col]))
      state_hosps <- state_hosps[order( state_hosps[,outcome_col], state_hosps[,2] ),]
      state_hosps <- na.omit(state_hosps)
      
      # find the row/rank to identify and report
      if(num == "best") {
        row_n = 1
      } else if (num == "worst") {
        row_n = nrow(state_hosps)
      } else {
        row_n = as.numeric(num)
      }
      
      # display the hospital name for valid inputs else 'NA'
      if(row_n >= 1 & row_n <= nrow(state_hosps)) {
        hosp_name <- state_hosps[row_n,2]
      } else {
        hosp_name <- "NA"
      }
      
      # collate each states result in a list
      resultlist[[valid_states[i]]] <- data.frame(hospital = hosp_name , state = valid_states[i] )
    }
    
    # create a dataframe to display the result
    result <- do.call(rbind, resultlist)

    return(result)
  }  
}