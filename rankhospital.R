# "state" can equal any correct two-letter abbreviations for a US State
# "outcome" can equal any of the following three strings:
#   1) "heart attack"
#   2) "heart failure"
#   3) "pneumonia"

rankhospital <- function(state = "MD", outcome = "heart failure", num = "best") {
  ## Read outcome data
  #setwd("/.../Intro to Data Science R Files/R Prog Assignment3-data")
  OutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = unique(OutcomeData[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## Return hospital name in that state with the given rank 30-day death rate
  DataByState <- OutcomeData[OutcomeData$State==state,]
  
  # order data by outcome
  DataByState.Sorted <- DataByState[order(as.numeric(DataByState[[colName]]),DataByState[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  #handle num input
  if (num=="best") num = 1
  if (num=='worst') num = nrow(DataByState.Sorted)
  #will automatically return NA if num > nrow, as well as if it's some other text value
  # if someone passes num < 1, they'll get what's expected
  #if (is.numeric(num) & num > nrwo(sorted.data.state) return(NA)
  
  DataByState.Sorted[num,"Hospital.Name"]
}