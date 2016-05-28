best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  # filter by input state
  stateOutcomeData <- outcomeData[which(outcomeData$State == state),];
  
  # create column name for input outcome
  mortalityColumnNamePrefix <- "Hospital.30.Day.Death..Mortality..Rates.from.";
  mortalityColumn <- paste(mortalityColumnNamePrefix, gsub(" ", ".", simpleCap(outcome)), sep = "");
  
  # coerce mortality column for input state to numeric
  stateOutcomeData[,mortalityColumn] <- suppressWarnings(as.double(stateOutcomeData[,mortalityColumn]));
  
  # find all hospitals with the lowest mortality rate for input outcome
  lowestMortalityHospitals <- stateOutcomeData$Hospital.Name[which(stateOutcomeData[mortalityColumn] == min(stateOutcomeData[mortalityColumn], na.rm = TRUE))];
  
  # sort list of hospitals alphabetically and return first hospital in set
  lowestMortalityHospitals[sort.list(lowestMortalityHospitals)][1];
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
