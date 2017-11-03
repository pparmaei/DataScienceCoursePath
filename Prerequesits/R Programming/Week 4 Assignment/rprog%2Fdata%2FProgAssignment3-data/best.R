#outcome-of-care-measuers.csv: contains information about 30-day mortality and readmisison rates for heat attacks, heart failure, and pneumonia for over 4,000 hostpital
#hospital-data.csv:Contains information about each hospital
#Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book), focus on page 19 number 11

best <- function(state, outcome){
        ##Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        allStates <- unique(outcomeData[,7])

        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        	stop("invalid outcome")
        }

        if (!state %in% allStates){
        	stop("invalid state")
        }    

      	if (outcome == "heart attack"){
      		colNumber <- 11
      	}
      	else if (outcome == "heart failure"){
      		colNumber <- 17
      	}
      	else if (outcome == "pneumonia"){
      		colNumber <- 23
      	}

      	outcomeData[,colNumber] <- as.numeric(outcomeData[,colNumber])
		    outcomeDataSplitByState <- split(outcomeData,outcomeData[,7])
		    outcomeofState <- outcomeDataSplitByState[[state]]
		    outcomeofState <- subset(outcomeofState, !is.na(outcomeofState[,colNumber]))
		    winnerHospital <- with(outcomeofState,outcomeofState[,2][ outcomeofState[,colNumber] == min(outcomeofState[,colNumber])])

        if(length(winnerHospital) > 1){
        	winnerHospital <- sort(winnerHospital)[1]
        }

        winnerHospital



        ## Return hospital $ name in that state with lowest 30-day death
        ## rate

        # with(df,df[,1][ y == min(df[,2])])

}

# 11 Hearattack, 17 Heart Failure, 23 pnemeunia
# Oucomes “heart attack”, “heart failure”, or “pneumonia”