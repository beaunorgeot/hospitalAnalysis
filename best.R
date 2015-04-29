#curl the data from "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip" to working dir and unzip it
# Here's the instructions for the project:
# https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2FProgAssignment3.pdf

#originally read the data in as character (by specifying colClasses = "character")

#Step 2. Choosing the best hospital in a state based on lowest mortality for certain disease
# See Instructions for details
#dt <- data.table(outcomes)
#Try setnames() in the data.table package. 
# Use something like setnames(DT,"b","B") or setnames(DT,c("a","E"),c("A","F")) 

#df <- data.frame(outcome)
# for data.frames you can use
# colnames(X)[c(1,2)] <- c("good", "better")
# since the col names are so long, df is actually easier
#colnames(df)[c(7,11,17,23)] <- c("state","heart attack", "heart failure","pneumonia")

best <- function(state, outcome) {
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
  # order all the rows based on the desired column(index=11,means based on the values of 'heart attack')
  # then take only the column 'Hospital Name'. 
  # The result is a vector w/the hospital names. The names appear ordered from lowest to highest rates of heart attacks
  slice <- slice[order(slice[,index]),"HosName"]
  #understanding the above
  #slice <- slice[what-to-order-on, range-of-columns-to-return]
  # slice <- slice[what-to-order-on,] returns the whole dataframe back, but with the rows ordered on whatever column you wanted
  # slice <- slice[order(slice[,index]),] // this returns the whole df back, ordered the outcome provided
  
  #Get hospital name with the lowest 30-day mortality rate.
  slice[1]
}