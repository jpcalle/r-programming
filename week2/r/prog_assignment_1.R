#
# Programming assignment 1: Air Pollution
#

setwd('D:/Dropbox/Coursera/data_science/02_r_programming/week2/r/')

# Part 1 -----------------------------------------------------------------------

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
  
  ids <- vector(mode = 'character', length = num_files)
  
  for (i in seq_along(id)) {
    if (id[i] < 10) {
      ids[i] <- paste('00', id[i], '.csv', sep = '')
    }
    else if (id[i] < 100 && id[i] > 9) {
      ids[i] <- paste('0', id[i], '.csv', sep = '')
    }
    else {
      ids[i] <- paste(id[i], '.csv', sep = '')
    }
  }
  
  data <- data.frame(sum_x = vector(mode = 'numeric', length = num_files),
                     num_x = vector(mode = 'integer', length = num_files))
  
  if (grepl('/$', directory)) {
    files <- paste('./', directory, ids, sep = '')
  } else{
    files <- paste('./', directory, '/', ids, sep = '')
  }
  
  for (i in seq_along(id)) {
    tmp_data <- read.csv(file = files[i], header = TRUE)
    data[i, 1] <- sum(tmp_data[, pollutant], na.rm = TRUE)
    data[i, 2] <- sum(!is.na(tmp_data[, pollutant]))
  }
  
  rm(tmp_data)
  
  return(sum(data$sum_x) / sum(data$num_x))
  
}

# Q1
pollutantmean("specdata", "sulfate", 1:10)

# Q2
pollutantmean("specdata", "nitrate", 70:72)

# Q3
pollutantmean("specdata", "sulfate", 34)

# Q4
pollutantmean("specdata", "nitrate")


# Part 2 -----------------------------------------------------------------------

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
  
  ids <- vector(mode = 'character', length = num_files)
  
  for (i in seq_along(id)) {
    if (id[i] < 10) {
      ids[i] <- paste('00', id[i], '.csv', sep = '')
    }
    else if (id[i] < 100 && id[i] > 9) {
      ids[i] <- paste('0', id[i], '.csv', sep = '')
    }
    else {
      ids[i] <- paste(id[i], '.csv', sep = '')
    }
  }
  
  data <- data.frame(id = vector(mode = 'numeric', length = num_files),
                     nobs = vector(mode = 'integer', length = num_files))
  
  if (grepl('/$', directory)) {
    files <- paste('./', directory, ids, sep = '')
  } else{
    files <- paste('./', directory, '/', ids, sep = '')
  }
  
  for (i in seq_len(num_files)) {
    tmp_data <- read.csv(file = files[i], header = TRUE)
    data[i, 1] <- id[i]
    comp_x1 <- !is.na(tmp_data[, 'sulfate'])
    comp_x2 <- !is.na(tmp_data[, 'nitrate'])
    data[i, 2] <- sum(comp_x1 * comp_x2)
  }
  
  rm(tmp_data)
  
  return(data)
  
}

# Q5
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

# Q6
cc <- complete("specdata", 54)
print(cc$nobs)

# Q7
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


# Part 3 -----------------------------------------------------------------------

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
  
  if (grepl('/$', directory)) {
    files <- paste(directory, list.files(path = directory), sep = '')
  } else{
    files <- paste(directory, '/', list.files(path = directory), sep = '')
  }
  
  correlations <- vector(mode = 'numeric', length = 0L)
  
  for (i in seq_len(num_files)) {
    tmp_data <- read.csv(file = files[i], header = TRUE)
    comp_x1 <- !is.na(tmp_data[, 'sulfate'])
    comp_x2 <- !is.na(tmp_data[, 'nitrate'])
    
    if (sum(comp_x1 * comp_x2) > threshold) {
      correlations <- c(correlations,
                        cor(tmp_data[, 'sulfate'], tmp_data[, 'nitrate'],
                            use = 'complete.obs'))
    }
  }
  
  rm(tmp_data)
  
  return(correlations)
  
}

# Q8
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

# Q9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

# Q10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
