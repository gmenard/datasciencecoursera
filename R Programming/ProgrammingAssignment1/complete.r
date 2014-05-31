complete <- function(directory, id = 1:332) {
    nobsNum <- numeric(0)
    for (cid in id) {
      # File name
      filename <- paste(directory, "/", sprintf("%03d", as.numeric(cid)), ".csv", sep = "")
      # Read File
      data <- read.csv(filename)
      # Calcul number of observed cases
      nobsNum <- c(nobsNum, nrow(na.omit(data)))
    }
    data.frame(id = id, nobs = nobsNum)  
}