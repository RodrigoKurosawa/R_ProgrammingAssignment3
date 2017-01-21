# 4 Ranking hospitals in all states
rankall <- function (outcome, num = "best") {
    # Read outcome data
    outcome_index <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    # Check that outcome is valid
    if (!(outcome %in% names(outcome_index)))
        stop("invalid outcome")
    
    #Getting the 3 columns that we want and removing the NA's
    df <- subset(df_outcome, select=c(2,7,outcome_index[outcome]))
    df <- na.omit(df)
    #Converting the 3rd column to numeric
    df[,3] <- as.numeric(df[,3])    
        
    #Get the list of States and create a new dataframe
    states <- unique(df_outcome[7])
    states <- sort(states[,1])
    hospitals <- data.frame(row.names=states,hospital=rep(NA,length(states)), state=states) 
        
    # For each state, find the hospital of the given rank
    for (i in 1:length(states)) {
        st <- states[i]
        #Getting the apropriate subset and sorting
        #The temp_df is the dataframe of the current state
        temp_df <- subset(df, State==st)
        temp_df <- temp_df[order(temp_df[3], temp_df[1]) , ]
        
        
        #Each state will have a 'ranks' vector
        ranks <- 1:nrow(temp_df)
        names(ranks)[1] <- "best"
        names(ranks)[nrow(temp_df)] <- "worst"
        
        #Putting the selected hospital in the hospitals dataframe
        selected_hospital <- temp_df[ranks[num],1]
        hospitals[i,1] <- selected_hospital
    }   
    # Return a data frame with the hospital names and the abbreviated state name
    hospitals
}