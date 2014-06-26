## Best function
## Author: Ali Hamidi
##
## R Programming - Programming Assignment 3

parseOutcome <- function(outcome) {
    
    # Should probably translate short outcome to full outcome
    # but there are only 3 valid outcomes anyway...
    
    if (outcome == "heart attack") {
        return("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
    } else if (outcome == "heart failure") {
        return("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
    } else if (outcome == "pneumonia") {
        return("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    }

}

best <- function(state, outcome) {
    ## Read outcome data
    ## Note: Set the NA string to "Not Available"
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    ## Check that state and outcome are valid
    states <- unique(data[, c("State")])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!any(state == states)) {
        # Provided state doesn't exist in data set
        stop("invalid state")
    } else if (!any(outcome == outcomes)) {
        # Provided outcome not included in valid outcomes vector
        stop("invalid outcome")
    } else {
        # Work out lowest for selected outcome
        
        # Translate outcome
        poutcome <- parseOutcome(outcome)
        
        # Subset data based on outcome and state
        sub_data <- data[data$State == state, c("Hospital.Name", poutcome)]
        
        # Exclude incomplete data
        sub_data <- sub_data[complete.cases(sub_data),]
        
        # Find row with lowest value
        min_row <- which.min(sub_data[,2])
        
        # Return Hospital Name of that row
        return(sub_data[min_row, 1])
    }
    
    
    
    
    return(states)
    
    ## Return hospital name in that state with lowest 30-day death rate
}