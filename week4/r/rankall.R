rankall <- function(outcome, num = "best") {
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
  
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    
    stop("invalid outcome")
    
  } else {
    
    # order handling ties
    ocare_ordered <- ocare_select[order(ocare_select[, outcome], ocare_select[, "hospital name"], ocare_select[, "state"]), ]
    
    splitted <- split(ocare_ordered, ocare_ordered[, "state"])
    
    if (is.numeric(num)) {
      return(as.data.frame(t(sapply(splitted, function(df) {
        if (num > nrow(df)) {
          na_df <- df[nrow(df), c("hospital name", "state")]
          na_df[1, "hospital name"] <- NA
          return(na_df)
        } else {
          return(df[num, c("hospital name", "state")])
        }
      }))))
    } else if (num == "worst") {
      return(as.data.frame(t(sapply(splitted, function(df) {
        df[nrow(df[!is.na(df[, outcome]), ]), c("hospital name", "state")]
      }))))
    } else {
      return(as.data.frame(t(sapply(splitted, function(df) {
        df[1, c("hospital name", "state")]
      }))))
    }
    
  }
  
}
