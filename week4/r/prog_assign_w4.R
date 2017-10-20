# Programming assignment week 4

hosp_data <- read.csv("hospital-data.csv")
out_care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(hosp_data)
head(out_care)
str(hosp_data)
str(out_care[, 11])  # heart attack

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

best("TX", "heart attack")  # CYPRESS FAIRBANKS MEDICAL CENTER
best("TX", "heart failure")  # FORT DUNCAN MEDICAL CENTER
best("MD", "heart attack")  # JOHNS HOPKINS HOSPITAL, THE
best("MD", "pneumonia")  # GREATER BALTIMORE MEDICAL CENTER

best("BB", "heart attack")  # inavlid state
best("NY", "hert attack")  # invalid outcome


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

rankhospital("TX", "heart failure", 4)  # DETAR HOSPITAL NAVARRO
rankhospital("MD", "heart attack", "worst")  # HARFORD MEMORIAL HOSPITAL
rankhospital("MN", "heart attack", 5000)  # NA
rankhospital("TX", "heart failure", "best")  # FORT DUNCAN MEDICAL CENTER


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

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)


## Quizz answers

# Q1
best("SC", "heart attack")

# Q2
best("NY", "pneumonia")

# Q3
best("AK", "pneumonia")

# Q4
rankhospital("NC", "heart attack", "worst")

# Q5
rankhospital("WA", "heart attack", 7)

# Q6
rankhospital("TX", "pneumonia", 10)

# Q7
rankhospital("NY", "heart attack", 7)

# Q8
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

# Q9
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

# Q10
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
