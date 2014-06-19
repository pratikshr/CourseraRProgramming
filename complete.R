complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector for the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    
    ##  Complete cases when both Nitrate and Sulfate observed on same day
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    wd <- '/Users/williammcdonald/datasciencecoursera/'
    Data <- data.frame()
    
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    ## loop through each file, count # of values (not NA's)
    for(file in (id)){        
        AllData <- read.csv(paste(wd,directory,'/',sprintf("%03d", file),".csv",sep=""))
        Data <- rbind(Data,cbind(file,nrow(AllData[complete.cases(AllData),])))
    }
    colnames(Data) <- c('id','nobs')
    Data
}