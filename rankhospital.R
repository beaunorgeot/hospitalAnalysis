# Returning hospitals by their ranking
# rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# rankhospital("MD", "heart failure", 5) // should return the 5th ranked hospital in MD for heart failure

#this is just the best.R() function that takes 1 additional parameter(num) and 
#returns the numTH index slice[num]

rankhospital <- function(state, outcome,num="best") {
  #Invalid outcome input type
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  #Get the column index for our given outcome string.
  # (heart attack,11), (heart failure 17), (pneumonia, 23)
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  #ifelse(if-to-test-for, if-true-do,else-do). The above statement is nested
  #ifelse(outcome==ha, if-yes-set-index-to-11, else-do-another-if-else)
  
  #Read and coerce our dataset while suppressing warnings and removing NA's.
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  data[,index] <- suppressWarnings(as.numeric(data[,index])) #set values in column of interest to numeric
  data <- na.omit(data)
  
  #Invalid state input or no observations
  states <- table(data$State)
  if (!state %in% names(states)) { 
    stop("invalid state")
  }
  
  #Slice our data by the given state and sort it by outcome and hospital name.
  #subset((dataFrame, rows2keep, columns2keep, drop = FALSE, ...)) -drop na's?
  slice <- subset(data, State==state,drop = TRUE) 
  colnames(slice)[c(2)] <- c("HosName")
  #order based on the number of $outcome observed AND the letters of the hospital names
  # Then take only the hospital names
  slice <- slice[order(slice[,index], slice[,2], na.last=TRUE),2] #this is ordered by rate
  
  # if num==best return index 1. if num==worst return last index. else return the index of the num given
  num <- ifelse(num == "best", 1, ifelse(num == "worst", length(slice), as.numeric(num)))
  
  #Get hospital name for the given rank by its 30-day mortality rate.
  slice[num]
}