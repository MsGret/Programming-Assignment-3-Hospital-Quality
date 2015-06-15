rankall <- function(outcome, num="best")
{
  # read outcome data (outcome-of-care-measures.csv)
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  rankhospital_dataframe = data.frame()
  #colnames(rankhospital_dataframe)[1] = "state"
  #colnames(rankhospital_dataframe)[2] = "hospital"
  # Check the state and outcome are valid :-
  #1. if state invalid throw an error via stop function "invalid state"
  #2. if outcome invalid throw an error via stop function "invalid outcome"
  
  outcome_existing <- c("heart attack","heart failure","pneumonia")
  # read the 30-day mortality rate for the given outcome and store it in a variable
  
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
  
  # return the name of the hospital(character) for the given city & the given outcome :-
  # 1. num = 3 specifies, 3rd lowest 30-day mortality rate. That is '3rd BEST'
  # 2. num = "worst" specifies the highest 30-day mortality rate. That is 'WORST'  
  # 3. num = "best" specifies the lowest 30-day mortality rate. That is 'BEST'
  # 4. if num larger than number of hospitals return NA
  # 5. ties must be handled alphabetically
  # 6. order can used to sort vectors
  for(i in 1:length(unique(outcome_data$State)))
  {
    state = sort(unique(outcome_data$State))[i]
    final_outcome_dataframe = subset(outcome_data,outcome_data$State == state)
    index_column <- which(names(final_outcome_dataframe) == lowest_outcome_30_day_mortality_rate)
    # not available removed, state = state, outcome = outcome. Just find the min index
    final_outcome_subset <- subset(final_outcome_dataframe, final_outcome_dataframe[,index_column]!="Not Available")
    final_outcome_numeric <- as.numeric(as.matrix(final_outcome_subset)[,index_column])
    final_outcome_sorted <- sort(final_outcome_numeric) # increasing order
    #print (final_outcome_sorted[num])
    #print(hospital_index)
    if(num =="best")
    {
      n = final_outcome_sorted[1]
      best_index = which(n == final_outcome_subset[,lowest_outcome_30_day_mortality_rate])
      hospital <- sort(as.character(final_outcome_subset$Hospital.Name[best_index]))[1]
      rankhospital_dataframe[i,1] = state
      rankhospital_dataframe[i,2] = hospital
      #return (as.character(final_outcome_subset$Hospital.Name[best_index])[1])
    }else if(num == "worst")
    {
      n = final_outcome_sorted[length(final_outcome_sorted)]
      worst_index = which(n == final_outcome_subset[,lowest_outcome_30_day_mortality_rate])
      hospital <- sort(as.character(final_outcome_subset$Hospital.Name[worst_index]))[1]
      rankhospital_dataframe[i,1] = state
      rankhospital_dataframe[i,2] = hospital
      #return (as.character(final_outcome_subset$Hospital.Name[worst_index])[1])
    }else
    {
      hospital_index = which(final_outcome_sorted[num] == final_outcome_subset[,lowest_outcome_30_day_mortality_rate])
      hospital <- sort(as.character(final_outcome_subset$Hospital.Name[hospital_index]))[1]
      rankhospital_dataframe[i,1] = state
      rankhospital_dataframe[i,2] = hospital
      #return (as.character(final_outcome_subset$Hospital.Name[hospital_index])[1])
    }
  }
  colnames(rankhospital_dataframe)[1] = "state"
  colnames(rankhospital_dataframe)[2] = "hospital"
  rankhospital_dataframe
}
