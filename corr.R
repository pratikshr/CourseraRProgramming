corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    wd <- getwd()
    Data <- data.frame()
    correlation <- numeric()
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    for(file in 1:332){        
        AllData <- na.omit(read.csv(paste(wd,'/',directory,'/',sprintf("%03d", file),".csv",sep="")))
        
        if(nrow(AllData) >= threshold){
            
            correlation<-c(correlation,cor(AllData$nitrate,AllData$sulfate))
        }
    }
    
    ## Return a numeric vector of correlations
    correlation
}