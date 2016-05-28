rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
  
  ## Check that outcome is valid
  
  validOutcomes <- c("heart attack", "heart failure", "pneumonia");
  if (!outcome %in% validOutcomes) {
    stop("invalid outcome");
  }
  
  ## For each state, find the hospital of the given rank
  
  # create column name for input outcome
  mortalityColumnNamePrefix <- "Hospital.30.Day.Death..Mortality..Rates.from.";
  mortalityColumn <- paste(mortalityColumnNamePrefix, gsub(" ", ".", simpleCap(outcome)), sep = "");
  
  # coerce mortality column for input state to numeric
  outcomeData[,mortalityColumn] <- suppressWarnings(as.double(outcomeData[,mortalityColumn]));
  
  # remove rows with NA value in mortalityColumn
  bad <- is.na(outcomeData[,mortalityColumn]);
  outcomeData <- outcomeData[!bad,];
  
  if (num == "best") {
    num <- 1;
  }
  
  isWorst <- FALSE
  if (num == "worst") {
    isWorst <- TRUE;
  }
  
  sortedByMortalityRateThenHospitalName <- outcomeData[order(outcomeData$State, outcomeData[,mortalityColumn], outcomeData$Hospital.Name),];
  
  returndf <- data.frame();
  for (state in unique(outcomeData$State)) {
    stateSorted <- sortedByMortalityRateThenHospitalName[which(sortedByMortalityRateThenHospitalName$State == state),];
    
    if (isWorst) {
      num <- nrow(stateSorted);
    }
    
    numRankedHospitalForState <- stateSorted[num,];
    stateResult <- cbind(numRankedHospitalForState$Hospital.Name, state);
    returndf <- rbind(returndf, stateResult);
  }
  
  rownames(returndf) <- returndf[,2];
  colnames(returndf) <- c("hospital", "state");
  
  returndf
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  
}
