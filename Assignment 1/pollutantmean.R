## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # get the full path of the directory
    fPath <- paste(getwd(), directory, sep = "/")
    
    # find all .csv files in the given directory
    files <- list.files(path = fPath, pattern="*.csv")
    
    # add the full path to all the .csv files
    files <- paste(fPath, files, sep = "/")
    
    # get the files in question
    fiq <- files[id]

    # pData <- read.csv(file, header = T)

    # fiq <- fiq[2:length(fiq)]

    # # loop through all files in question
    # for(file in fiq)
    # {
    #     tempData <- read.csv(file, header = F)
    #     pData <- rbind(pData, tempData)
    #     rm(tempData)
    # }


    # for(i <- 1; i < length(fiq); i++)
    # {
    #     tempData <- read.csv(fiq[i], header = F)
    #     pData <- rbind(pData, tempData)
    #     rm(tempData)
    # }
 


    # loop through all files in question
    for(file in fiq)
    {
        # if pData doesn't exist, create it
        if(!exists("pData"))
        {
            pData <- read.csv(file, header = T)
        }
        
        # if pData exist, bind it with the last pData
        else
        {
            tempData <- read.csv(file, header = T)
            pData <- rbind(pData, tempData)
#             rm(tempData)
        }
    }
    
    # returning the mean of pollutant data
    pollutantMean <- mean(pData[[pollutant]], na.rm = TRUE)
    
    # round the value to only have 3 decimal points
    round(pollutantMean, digits = 3)
}