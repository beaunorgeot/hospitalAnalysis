#Get the hospital names from each state that correspond to a given rank
# rankall takes two arguments: an outcome name (outcome) and a hospital ranking(num).
#rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states

rankall <- function(outcome, num="best") {
  #Invalid outcome input type
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  #Get index for our given outcome string.
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  #Read and coerce our dataset while suppressing warnings and removing NA's.
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  data <- data[!is.na(data[,index]),]
  
  #Sort our data by specified mortality rate and hospital name
  data.sorted <- data[order(data[,index], data[,2], na.last=TRUE),]
  data.sorted <- na.omit(data.sorted)
  
  #Parse out and validate our num
  num <- ifelse(num == "best", 1, ifelse(num == "worst", length(data.sorted), as.numeric(num)))
  
  #Remove duplicate state names
  states <- sort(unique(data.sorted[,7]))
  
  #Function returns the hospital name (col2) for the given state(col7) at the specified rank.
  state_hospital_data <- function(state) {
    #take only the rows that from the state we're interested in. Data already ordered on outcome/disease
    slice <- subset(data.sorted, State==state)
    # take only the row corresponding to the rank we want, and take only the hosName, hosState
    slice <- slice[num, c(2,7)]
    #change the State column name for the resulting df to the input state
    slice$State <- state
    return (slice)
  }
  #for each state in states, apply the state_hospital_data() function
  #state_data <- lapply(states, state_hospital_data)
  #for each state in states, apply the state_hospital_data() function
  #rbind the results together, set the names of the rows to be the names of the states
  state_data <- do.call(rbind, lapply(states, state_hospital_data))
  row.names(state_data) <- states
  colnames(state_data) <- c("hospital", "state")
  return (state_data)
}