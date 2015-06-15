best <- function(state,outcome) 
{
  # read outcome data (outcome-of-care-measures.csv)
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  # Check the state and outcome are valid :-
  #1. if state invalid throw an error via stop function "invalid state"
  #2. if outcome invalid throw an error via stop function "invalid outcome"
  if(is.na(match(outcome,outcome_existing)))
  {
    stop("invalid outcome") 
  }
  
  if(is.na(match(state, unique(outcome_data$State))))
  {
    stop("invalid state")
  }
  
  # Return the hospital name(string) in that state with lowest 30 day mortaility rate for that outcome :-
  #1. if na ignore that hospital
  #2. if tie decide the best hospital alphabetically
  #where outcome_data$Hospital.Name and  
  #"Hospital.30.Day.Death..Mortality..Rates.from./Heart.Attack/Heart.Failure/Pneumonia" 
  #are hospital name and 30 day mortality rate of a given outcome
  outcome_existing <- c("heart attack","heart failure","pneumonia")
  if(outcome == outcome_existing[1])
  {
    input = "Heart.Attack"
  }else if(outcome == outcome_existing[2])
  {
    input = "Heart.Failure"
  }else 
  {
    input = "Pneumonia"
  }
  lowest_outcome_30_day_mortality_rate <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",input,sep="")
  final_outcome_dataframe = subset(outcome_data,outcome_data$State == state)
  index_column <- which(names(final_outcome_dataframe) == lowest_outcome_30_day_mortality_rate)
  hospital_index <- suppressWarnings(which.min(as.numeric(as.matrix(final_outcome_dataframe)[,index_column])))
  # removes the warning NAs introduced by coercion
  as.character(final_outcome_dataframe$Hospital.Name[hospital_index])
}
