# Read data
names(outcome_data)
str(outcome_data[, 11])

best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(is.element(state, outcome_data$State)  ){
        # Cast the column death rate by heart attack to numeric
        outcome_data[, 11] <- as.numeric(outcome_data[, 11])
        outcome_data[, 17] <- as.numeric(outcome_data[, 17])
        outcome_data[, 23] <- as.numeric(outcome_data[, 23])
        
        
         # Subset the data by state
        data_state = outcome_data[outcome_data$State == state, ]
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if(outcome == "heart attack"){
            #names(data_state)
            data_state[order(data_state[, 11]), ]$Hospital.Name[1] 
        }
        else if(outcome == "heart failure"){
            data_state[order(data_state[, 17]), ]$Hospital.Name[1] 
        }
        else if(outcome == "pneumonia"){
            data_state[order(data_state[, 23]), ]$Hospital.Name[1] 
        }
        else {
            stop("invalid outcome")
        }
    }
    else{
        stop("invalid state")
    }
   
}
