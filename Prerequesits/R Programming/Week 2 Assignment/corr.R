corr <- function (directory, threshold = 0) {
        fileList <- list.files(directory)
        currCorr <- numeric()
        for (filename in fileList){
                currData <- read.csv(paste(directory,"/",filename,sep=""))
                currCompleteData <- subset(currData,!is.na(sulfate)&!is.na(nitrate))
                currCompleteCount <- nrow(currCompleteData)
                if (currCompleteCount > threshold){
                        currSulfate <- currCompleteData[,"sulfate"]
                        currNitrate <- currCompleteData[,"nitrate"]
                        currCorr <- append(currCorr,cor(currNitrate,currSulfate))
                }
        }
        currCorr
}