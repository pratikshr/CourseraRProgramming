pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector for the location of the CSV files 
    ##  example == /Users/williammcdonald/datasciencecoursera/specdata/
    
    ## need to loop through id.csv, read file and add to an empty data frame
    ## row wise for both pollutants
    
    Data <- data.frame()
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    ##  format as 3 character value ("001", "002", etc )      
    
    for(file in (id)){
        Data<-rbind(Data,read.csv(paste('/Users/williammcdonald/datasciencecoursera/',directory,'/',sprintf("%03d", file),".csv",sep="")))
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
}
mean(as.matrix(Data[pollutant]),na.rm=TRUE)
}