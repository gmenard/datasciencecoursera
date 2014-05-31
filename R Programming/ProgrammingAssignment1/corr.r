corr <- function(directory, threshold = 0) {
  files <- list.files(directory)
  
  res <- c() 
  
  for(f in 1:length(files)){
    # File name
    filename <- paste(directory, "/", files[f], sep="")
    # Read File
    data <- read.csv(filename)
    # Build data frame
    data <- data[complete.cases(data),]
    # Compute correlation
    if ( nrow(data) > threshold ) {
      res <- c(res, cor(data$sulfate, data$nitrate) )
    }
  }
  
  return(res)
}