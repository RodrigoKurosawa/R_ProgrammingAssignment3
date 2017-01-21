rankhospital <- function(state, outcome, num = "best") {
    #Read outcome data
    outcome_index <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")

    # Check that state and outcome are valid
    ## state = col 7 ; outcome = col 11,17,23
    if (!(state %in% df_outcome[,7]))
        stop("invalid state")
    if (!(outcome %in% names(outcome_index)))
        stop("invalid outcome")
    
    #Getting the 3 columns that we want and removing the NA's
    df <- subset(df_outcome, State==state, select=c(2,7,outcome_index[outcome]))
    df <- na.omit(df)
    #Converting the 3rd column to numeric
    df[,3] <- as.numeric(df[,3])    
    #Sorting the data frame
    df <- df[order(df[3], df[1]) , ]
        
    # Return hospital name in that state with the given rank 30-day death rate
    ranks <- 1:nrow(df)
    names(ranks)[1] <- "best"
    names(ranks)[nrow(df)] <- "worst"
    df[ranks[num],1]
}