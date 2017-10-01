complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  num_files <- length(id)
  
  data <- data.frame(id = vector(mode = 'numeric', length = num_files),
                     nobs = vector(mode = 'integer', length = num_files))
  
  if (grepl('/$', directory)){
    files <- paste(directory, sprintf('%003i', id), '.csv', sep = '')
  } else{
    files <- paste(directory, '/', sprintf('%003i', id), '.csv', sep = '')
  }
  
  for (i in seq_len(num_files)){
    tmp_data <- read.csv(file = files[i], header = TRUE)
    data[i, 1] <- id[i]
    comp_x1 <- !is.na(tmp_data[, 'sulfate'])
    comp_x2 <- !is.na(tmp_data[, 'nitrate'])
    data[i, 2] <- sum(comp_x1 * comp_x2)
  }
  
  rm(tmp_data)
  
  return(data)
  
}