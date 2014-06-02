rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% unique(data[, "State"])) stop("invalid state")
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

  ## Return hospital name in that state with the given rank 30-day death rate
 
  # Filter by State
  data <- data[data$"State" == state,]
  
  # Remove NA values
  data[,outcomeColName] <- as.numeric(data[,outcomeColName])
  data <- data[!is.na(data[,outcomeColName]),]
  
  # Sort by rate and hospital name
  sortDecreasing = FALSE
  if(num == "best") {
    num = 1
  }
  else if(num == "worst") {
    sortDecreasing = TRUE
    num = 1
  }
  data <- data[order(data[,outcomeColName], data[,"Hospital.Name"], decreasing=sortDecreasing), ]
  
  data$"Hospital.Name"[num]
}
