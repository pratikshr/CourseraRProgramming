# Develop a function to determine Hospital with lowest 30 day mortality rates
# by State by Outcome (Heart attack, heart failure, pneumonia)
# Check for misentered State ID' and mistyped outcomes
# Same outcome results should return firt alphabetically


# define the function
best <- function(state,outcome){
        # read data
        data <- read.csv("outcome-of-care-measures.csv")
        df <- data[,c(2,7,11,17,23)]
        names(df) <- c('Hospital','State','heart attack','heart failure','pneumonia')
       
        # develop list of states and outcomes
        valid_states <- unique(df[,2])
        valid_outcomes <- c('heart attack','heart failure','pneumonia')

        #check the validity of State and Outcome entries
        if (!(state %in% valid_states)){
                stop('invalid state')
        }
        if (!(outcome %in% valid_outcomes)){
                stop('invalid outcome')
        }
        # subset data by State
        df_state <- df[df$State == state, ]
        # transform numbers from characters
        df_state[, c(3,4,5)] <- sapply(df_state[, c(3,4,5)], as.numeric)
        # place hospitals in alphabetical order (necessary ???)
        # df_state <- df_state[order(df2[, 1]), ]
        
        # find minimum for each outcome by state
        if (outcome == 'heart attack'){
                best <- df_state[which.min(df_state[, 3]), "Hospital"]
        }
         
        else if (outcome == 'heart failure'){
                best <- df_state[which.min(df_state[, 4]), "Hospital"] 
        }
        
        else {
                best <- df_state[which.min(df_state[, 5]), "Hospital"]
        }
        
        best
        
}