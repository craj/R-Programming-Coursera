rankhospital <- function(state, outcome, num = "best") {
  
  # load the data set into hosp_data
  hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # list of unique and valid states
  valid_states <- unique(c(hosp_data$State))
  
  # list of valid outcomes
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!state %in% valid_states) {
    stop("invalid state")
  } else if(!outcome %in% valid_outcomes) {
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
    
    # subset of data for the entered 'state'
    state_hosps <- hosp_data[hosp_data$State == state,]
    
    # convert data to numeric for sorting (and suppress NA coercion)
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
    return(hosp_name)
  }  
}