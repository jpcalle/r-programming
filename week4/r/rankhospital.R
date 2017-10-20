rankhospital <- function(state, outcome, num) {
  # returns the num hospital for ranking of best outcome var for the given state
  
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
    
    # order handling ties
    ocare_ordered <- ocare_filter[order(ocare_filter[, outcome], ocare_filter[, "hospital name"]), ]
    
    if (is.numeric(num)) {
      return(ocare_ordered[num, "hospital name"])
    } else if (num == "worst") {
      return(ocare_ordered[nrow(ocare_ordered), "hospital name"])
    } else {
      return(ocare_ordered[1, "hospital name"])
    }
    
  }
  
}
