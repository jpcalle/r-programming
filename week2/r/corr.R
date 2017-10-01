corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  num_files <- length(list.files(path = directory))
  
  if (grepl('/$', directory)){
    files <- paste(directory, list.files(path = directory), sep = '')
  } else{
    files <- paste(directory, '/', list.files(path = directory), sep = '')
  }
  
  correlations <- vector(mode = 'numeric', length = 0L)
  
  for (i in seq_len(num_files)){
    tmp_data <- read.csv(file = files[i], header = TRUE)
    comp_x1 <- !is.na(tmp_data[, 'sulfate'])
    comp_x2 <- !is.na(tmp_data[, 'nitrate'])
    
    if (sum(comp_x1 * comp_x2) > threshold){
      correlations <- c(correlations,
                        cor(tmp_data[, 'sulfate'], tmp_data[, 'nitrate'],
                            use = 'complete.obs'))
    }
  }
  
  rm(tmp_data)
  
  return(correlations)
  
}