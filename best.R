# Users should set the working directory to their own preferred location 
# github link:https://github.com/acblimehouse/C_DSC_RProg

# "state" can equal any correct two-letter abbreviations for a US State
# "outcome" can equal any of the following three strings:
#   1) "heart attack"
#   2) "heart failure"
#   3) "pneumonia"

best <- function(state = "MD", outcome = "heart failure") {
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  OutcomeByState <- OutcomeData[OutcomeData$State==state,]
  idx <- which.min(as.double(OutcomeByState[,colName]))
  OutcomeByState[idx,"Hospital.Name"]
}
