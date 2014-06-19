## Return a dataframe of the Hospitals in all states that match ranking number
## Hospital  State
rankall <- function(outcome,num = 'best' ) {
        ## Read outcome of care data
        data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character')
        ## subset for relevant columns
        df <- data[,c(2,7,11,17,23)]
        ## Rename columns for simplicity
        names(df) <- c('Hospital','State','heart attack','heart failure','pneumonia')
        
        # develop list of states and outcomes
        states <- unique(df[,2])
        outcomes <- c('heart attack','heart failure','pneumonia')
        
        ## check the validity of Outcome entries
        if (!(outcome %in% outcomes)){
                stop('invalid outcome')
        }
        
        ## transform characters to numbers in relevant columns
        df[, c(3,4,5)] <- suppressWarnings(sapply(df[, c(3,4,5)], as.numeric))
        
        #empty data frame
        ranks <- data.frame(hospital=NA, state=NA)
        ## For each state, find the hospital of the given rank        
        for (int in 1:length(states)){
                ## Using rankhospital function, create dataframe for each state
                ranks[int, ] <- c(rankhospital(states[int], outcome, num), states[int])
                ## ??? do i need to reread the data through this function ???
        }
        
        ranks
        
}       
