complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector for the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
        
    ##  Complete cases when both Nitrate and Sulfate observed on same day
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    
    Data <- data.frame()
    Data2 <- data.frame()    
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    ## loop through each file, count # of values (not NA's)
    for(file in (id)){
        Data<-rbind(Data,sum(complete.cases(read.csv(paste(directory,sprintf("%03d", file),".csv",sep="")))))
        Data2 <- rbind(Data,read.csv(paste(directory,sprintf("%03d", file),".csv",sep="")))
}
colnames(Data) <- 'nobs'

Data
}