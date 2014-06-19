## Define a function to find any hospital with the 
## desired rank within a particular State
rankhospital <- function(state,outcome,num = 'best'){
        ## redefine special cases?
        
        ## read data
        data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character')
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
        ## transform numbers from characters
        df_state[, c(3,4,5)] <- suppressWarnings(sapply(df_state[, c(3,4,5)], as.numeric))
        ## place hospitals in alphabetical order (necessary ???)
        ## df_state <- df_state[order(df2[, 1]), ]
        df_state <- df_state[order(df_state[,2]),]
        
        ## find minimum for each outcome by state
        if (outcome == 'heart attack'){
                ordered_state <- df_state[suppressWarnings(order(x<-df_state[,3], y<- df_state[,1], na.last = NA)),]
                if (num =='best'){num <- 1}
                else if (num == 'worst'){num <- nrow(ordered_state)}
                ordered <- ordered_state[num,'Hospital']
        }
        
        else if (outcome == 'heart failure'){
                ordered_state <- df_state[order(x<-df_state[,4], y<- df_state[,1], na.last = NA),]
                if (num =='best'){num <- 1}
                else if (num == 'worst'){num <- nrow(ordered_state)}
                ordered <- ordered_state[num,'Hospital']
        }
        
        else {
                ordered_state <- df_state[order(x<-df_state[,5], y<- df_state[,1], na.last = NA),]
                if (num =='best'){num <- 1}
                else if (num == 'worst'){num <- nrow(ordered_state)}
                ordered <- ordered_state[num,'Hospital']
        }
        
        ordered
        
}