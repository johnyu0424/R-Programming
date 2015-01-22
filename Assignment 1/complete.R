## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) 
{
    # get the full path of the directory
    fPath <- paste(getwd(), directory, sep = "/")
    
    # find all .csv files in the given directory
    files <- list.files(path = fPath, pattern="*.csv")
    
    # add the full path to all the .csv files
    files <- paste(fPath, files, sep = "/")
    
    # get the files in question
    fiq <- files[id]
    
    # initialize nobs to have some dummy data, will be remove later
    nobs <- 0
    
    # loop through all files in question
    for(file in fiq)
    {
        # load the data into pData
        pData <- read.csv(file, header = T)
        
        # using complete.cases to find all data that contain complete data
        # the function return a logical vector
        completeData <- complete.cases(pData)
        
        # from the logical vector count all the elements that are TRUE, which is
        # the count of all complete cases.
        numCompleteData <- length(completeData[completeData == TRUE])
        
        # add the count into nobs vector
        nobs <- c(nobs, numCompleteData)
    }
    
    # remove the first dummy data that was added earlier
    nobs <- nobs[-1]
    
    # creating the data frame using id and nobs
    completeDataFrame <- data.frame(id, nobs)
    
    # return the complete data frame
    completeDataFrame
}