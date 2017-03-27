# "state" can equal any correct two-letter abbreviations for a US State
# "outcome" can equal any of the following three strings:
#   1) "heart attack"
#   2) "heart failure"
#   3) "pneumonia"

rankall <- function(outcome = "heart attack", num = "best") {
  ## Read outcome data
  OutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = unique(OutcomeData[,7])
  #if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## For each state, find the hospital of the given rank
  hospital<-character(0)
  
  for (i in seq_along(validState)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    DataByState <- OutcomeData[OutcomeData$State==validState[i],]
    
    # order data by outcome
    DataByState.Sorted <- DataByState[order(as.numeric(DataByState[[colName]]),DataByState[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    #handle num input
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(DataByState.Sorted)
    
    hospital[i] <- DataByState.Sorted[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital=hospital,state=validState,row.names=validState)
}