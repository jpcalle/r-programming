pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  num_files <- length(id)

  data <- data.frame(sum_x = vector(mode = 'numeric', length = num_files),
                     num_x = vector(mode = 'integer', length = num_files))
  
  if (grepl('/$', directory)){
    files <- paste(directory, sprintf('%003i', id), '.csv', sep = '')
  } else{
    files <- paste(directory, '/', sprintf('%003i', id), '.csv', sep = '')
  }
  
  for (i in seq_len(num_files)){
    tmp_data <- read.csv(file = files[i], header = TRUE)
    data[i, 1] <- sum(tmp_data[, pollutant], na.rm = TRUE)
    data[i, 2] <- sum(!is.na(tmp_data[, pollutant]))
  }
  
  rm(tmp_data)
  
  return(sum(data$sum_x) / sum(data$num_x))
  
}
