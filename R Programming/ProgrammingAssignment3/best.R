best <- function(state, outcome) {
  
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
 
  ## Return hospital name in that state with lowest 30-day death rate
  
  # Filter by State
  data <- data[data$"State" == state,]

  # Remove NA values
  data[,outcomeColName] <- as.numeric(data[,outcomeColName])
  data <- data[!is.na(data[,outcomeColName]),]
  
  # Sort by rate and hospital name
  data <- data[order(data[,outcomeColName], data[,"Hospital.Name"], decreasing=FALSE), ]
  
  data$"Hospital.Name"[1]
}