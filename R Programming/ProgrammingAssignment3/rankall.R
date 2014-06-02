rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  switch(outcome,
         "heart attack" = {
           outcomeColName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
         },
         "heart failure" = {
           outcomeColName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
         },
         "pneumonia" = {
           outcomeColName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
         },
         {
           stop("invalid outcome")
         }
  )
    
  # Remove NA values
  data[,outcomeColName] <- as.numeric(data[,outcomeColName])
  data <- data[!is.na(data[,outcomeColName]),]
  
  # Set sorter according to rank
  if(num == "best") {
    sortDecreasing = FALSE
    num = 1
  }
  else if(num == "worst") {
    sortDecreasing = TRUE
    num = 1
  }
  
  ## For each state, find the hospital of the given rank

  state <- c()
  hospital <- c()
  
  # Go through states
  for(s in sort(unique(data[, "State"]))) {
    
    # Filter Hospitals by State
    data1 <- data[data$"State" == s,]
    
    # Sort data by rate and hospitals
    data1 <- data1[order(data1[,outcomeColName], data1[,"Hospital.Name"], decreasing=sortDecreasing), ]
    
    # Save state and hospital name
    state <- c(state, s)
    hospital <- c(hospital, data1$"Hospital.Name"[num])
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital, state)
}