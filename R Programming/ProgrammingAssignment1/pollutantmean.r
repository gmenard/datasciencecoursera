pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # Get list of files in directory
  files.all = list.files(directory)
  
  # extract the file names and store as numeric for comparison
  files.names = as.numeric(sub("\\.csv", "", files.all))
  
  # Select files to be imported
  files.selected = files.all[match(id, files.names)]
  
  # Retrieve files data
  data = lapply(file.path(directory, files.selected), read.csv)
  data = do.call(rbind.data.frame, data)
  
  # Get mean
  mean(data[ , pollutant], na.rm=TRUE)
  
}
