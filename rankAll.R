

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    # Cast the column death rate by heart attack to numeric
    outcome_data[, 11] <- as.numeric(outcome_data[, 11])
    outcome_data[, 17] <- as.numeric(outcome_data[, 17])
    outcome_data[, 23] <- as.numeric(outcome_data[, 23])
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if(outcome == "heart attack"){
        #names(data_state)
        if(is.numeric(num)){
            
            hospital <- outcome_data[order(outcome_data[, 11], outcome_data[, 2]), ]$Hospital.Name
            state <- outcome_data[order(outcome_data[, 11], outcome_data[, 2]), ]$State
            data_result <- data.frame(hospital, state)
            data_result
        }
        else{ 
            if( num == "worst"){
                outcome_data[order(outcome_data[, 11], outcome_data[, 2], decreasing = TRUE),]$Hospital.Name[1]
            }
            else{
                outcome_data[order(outcome_data[, 11], outcome_data[, 2]), ]$Hospital.Name[1] 
            }    
        }
        
    }
    else if(outcome == "heart failure"){
        if(is.numeric(num)){
            outcome_data[order(outcome_data[, 17], outcome_data[, 2]), ]$Hospital.Name[num]
        }
        else{ 
            if( num == "worst"){
                outcome_data[order(outcome_data[, 17], outcome_data[, 2], decreasing = TRUE), ]$Hospital.Name[1]
            }
            else{
                outcome_data[order(outcome_data[, 17], outcome_data[, 2]), ]$Hospital.Name[1] 
            }    
        }
        
    }
    else if(outcome == "pneumonia"){
        if(is.numeric(num)){
            outcome_data[order(outcome_data[, 23], outcome_data[, 2]), ]$Hospital.Name[num] 
        }
        else{ 
            if( num == "worst"){
                outcome_data[order(outcome_data[, 23], outcome_data[, 2], decreasing = TRUE), ]$Hospital.Name[1]
            }
            else{
                outcome_data[order(outcome_data[, 23], outcome_data[, 2]), ]$Hospital.Name[1] 
            }    
        }
    }
    else {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
}