complete <- function(directory, id=1:332){
        df <- data.frame(matrix(ncol=2, nrow = 0))
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
                currCompleteData <- subset(currData,!is.na(sulfate)&!is.na(nitrate))
                currCompleteCount <- nrow(currCompleteData)
                df <- rbind(df, c(number,currCompleteCount))
        }
        
        colnames(df) <- c("id", "nobs")
        df
}
