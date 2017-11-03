#outcome-of-care-measuers.csv: contains information about 30-day mortality and readmisison rates for heat attacks, heart failure, and pneumonia for over 4,000 hostpital
#hospital-data.csv:Contains information about each hospital
#Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book), focus on page 19 number 11

rankall <- function(outcome, num = "best"){
        ##Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        ## Check that state and outcome are valid
        allStates <- unique(outcomeData[,7])

        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop("invalid outcome")
        }

        ## For each state, find the hospital of the given rank
        ## Return a data frame with teh hospital names and the (abbreviated) state name


        ## Return hospital name in that state with teh given rank
        ## 30-day death rate          
  
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
        hospitalsArray <- character()

        for (state in allStates){
  		    outcomeDataSplitByState <- split(outcomeData,outcomeData[,7])
  		    outcomeofState <- outcomeDataSplitByState[[state]]
  		    outcomeofState <- subset(outcomeofState, !is.na(outcomeofState[,colNumber]))

          Hospitalrank <- order(outcomeofState[,colNumber],outcomeofState[,2])

          if (num == "best"){
            ranktoReturn <- 1
          }
          else if(num == "worst"){
            ranktoReturn <- length(Hospitalrank)
          }
          else{
            ranktoReturn <- num
          }
          
          hospitalsArray <- append(hospitalsArray,outcomeofState[Hospitalrank,][ranktoReturn,2])

      }

      dfResult <- data.frame(hospital=hospitalsArray,state=allStates)
      dfResult <- dfResult[order(dfResult[,2]),]
      dfResult


		    #winnerHospital <- with(outcomeofState,outcomeofState[,2][ outcomeofState[,colNumber] == min(outcomeofState[,colNumber])])

        # if(length(winnerHospital) > 1){
        # 	winnerHospital <- sort(winnerHospital)[1]
        # }

        # winnerHospital

        ## Return hospital $ name in that state with lowest 30-day death
        ## rate

        # with(df,df[,1][ y == min(df[,2])])

}

# 11 Hearattack, 17 Heart Failure, 23 pnemeunia
# Oucomes “heart attack”, “heart failure”, or “pneumonia”