pollutantmean <- function(directory, pollutant, id = 1:332){
        currSum <- 0
        currLength <- 0
        for (number in id){
                if (number <10){
                        filename <- paste0("00", number)
                }
                else if (number < 100){
                        filename <- paste0("0", number)
                }
                else if (number < 1000){
                        filename <- paste0("", number)
                }
                currData <- read.csv(paste(directory,"/",filename,".csv",sep=""))
                currSum <- currSum+sum(subset(currData,!is.na(currData[,pollutant]))[,pollutant])
                currLength <-currLength +length(subset(currData, !is.na(currData[,pollutant]))[,pollutant])
        }
        currSum/currLength
}