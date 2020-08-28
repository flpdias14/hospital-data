
rankhospital <- function(state, outcome, num = "best") {
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
            if(is.numeric(num)){
                data_state[order(data_state[, 11], data_state[, 2]), ]$Hospital.Name[num]
            }
            else{ 
                if( num == "worst"){
                    data_state[order(data_state[, 11], data_state[, 2], decreasing = TRUE),]$Hospital.Name[1]
                }
                else{
                    data_state[order(data_state[, 11], data_state[, 2]), ]$Hospital.Name[1] 
                }    
            }
           
        }
        else if(outcome == "heart failure"){
            if(is.numeric(num)){
                data_state[order(data_state[, 17], data_state[, 2]), ]$Hospital.Name[num]
            }
            else{ 
                if( num == "worst"){
                    data_state[order(data_state[, 17], data_state[, 2], decreasing = TRUE), ]$Hospital.Name[1]
                }
                else{
                    data_state[order(data_state[, 17], data_state[, 2]), ]$Hospital.Name[1] 
                }    
            }
            
        }
        else if(outcome == "pneumonia"){
            if(is.numeric(num)){
                data_state[order(data_state[, 23], data_state[, 2]), ]$Hospital.Name[num] 
            }
            else{ 
                if( num == "worst"){
                    data_state[order(data_state[, 23], data_state[, 2], decreasing = TRUE), ]$Hospital.Name[1]
                }
                else{
                    data_state[order(data_state[, 23], data_state[, 2]), ]$Hospital.Name[1] 
                }    
            }
            
        }
        else {
            stop("invalid outcome")
        }
    }
    else{
        stop("invalid state")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
}