best <- function(state, outcome) {
  # returns the best hospital in the state for a given outcome measure
  
  # reading data
  ocare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cols <- c(2, 7, 11, 17, 23)
  
  # coerce to numeric
  suppressWarnings({
    ocare[, 11] <- as.numeric(ocare[, 11])
    ocare[, 17] <- as.numeric(ocare[, 17])
    ocare[, 23] <- as.numeric(ocare[, 23])
  })  # do not show NA warnings coertions
  
  # select only needed cols and filter desired rows
  ocare_select <- ocare[, cols]
  names(ocare_select) <- c("hospital name", "state", "heart attack",
                           "heart failure", "pneumonia")
  
  if (!(state %in% ocare_select[, "state"])) {
    
    stop("invalid state")
    
  } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    
    stop("invalid outcome")
    
  } else {
    
    # filter desired rows
    ocare_filter <- ocare_select[!is.na(ocare_select[, outcome]) & ocare_select[, "state"] == state, ]
    
    order_index <- sort(ocare_filter[, outcome],
                        index.return = TRUE)
    
    # sorted
    lower_outcome <- ocare_filter[order_index$ix, outcome][1]
    
    # handling ties
    ocare_filt_lower <- ocare_filter[ocare_filter[, outcome] == lower_outcome, ]
    
    order_index_name <- sort(ocare_filt_lower[, "hospital name"],
                             index.return = TRUE)
    
    return(ocare_filt_lower[order_index_name$ix, "hospital name"][1]) 
    
  }
  
}
