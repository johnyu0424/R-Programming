## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) 
{
    # get the complete data count from "complete()"
    completeData <- complete(directory)
    
    # get the full path of the directory
    fPath <- paste(getwd(), directory, sep = "/")
    
    # find all .csv files in the given directory
    files <- list.files(path = fPath, pattern="*.csv")
    
    # add the full path to all the .csv files
    files <- paste(fPath, files, sep = "/")
    
    # finding all the rows that have nob above the threshold
    completeDataAboveThreshold <- completeData[(completeData$nob > threshold), ]
    
    # get the file id of the rows that have nob above the threshold
    id <- completeDataAboveThreshold$id
    
    # get the files in question
    fiq <- files[id]
    
    # initialize id to have some dummy data, will be remove later
    corData <- 0
    
    # loop through all files in question
    for(file in fiq)
    {
        # load the data into pData
        pData <- read.csv(file, header = T)
        
        corData <- c(corData, cor(pData$sulfate, pData$nitrate, use = "complete.obs"))
    }
    
    # remove the first dummy data that was added earlier
    corData <- corData[-1]
    
    # return the correlation data
    corData
}