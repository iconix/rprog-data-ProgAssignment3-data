rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
  
  ## Check that state and outcome are valid
  
  validOutcomes <- c("heart attack", "heart failure", "pneumonia");
  if (!outcome %in% validOutcomes) {
    stop("invalid outcome");
  }
  
  if (!state %in% unique(outcomeData$State)) {
    stop("invalid state");
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  # filter by input state
  stateOutcomeData <- outcomeData[which(outcomeData$State == state),];
  
  # create column name for input outcome
  mortalityColumnNamePrefix <- "Hospital.30.Day.Death..Mortality..Rates.from.";
  mortalityColumn <- paste(mortalityColumnNamePrefix, gsub(" ", ".", simpleCap(outcome)), sep = "");
  
  # coerce mortality column for input state to numeric
  stateOutcomeData[,mortalityColumn] <- suppressWarnings(as.double(stateOutcomeData[,mortalityColumn]));
  
  # remove rows with NA value in mortalityColumn
  bad <- is.na(stateOutcomeData[,mortalityColumn]);
  stateOutcomeData <- stateOutcomeData[!bad,];
  
  sortedByMortalityRateThenHospitalName <- stateOutcomeData[order(stateOutcomeData[,mortalityColumn], stateOutcomeData$Hospital.Name),];
  
  # return hospital name in position of the input rank
  if (num == "best") {
    num <- 1;
  }
  if (num == "worst") {
    num <- nrow(sortedByMortalityRateThenHospitalName);
  }
  sortedByMortalityRateThenHospitalName$Hospital.Name[num];
}
