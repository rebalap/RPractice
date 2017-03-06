best <- function(state, outcome) {
  ## Read outcome data
  outcome_data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  column<-numeric()
  #hospital_name<-data.frame()
  #check for states
  if((state %in% outcome_data$State) == FALSE)
  {
    stop("invalid state")
  }
  #check for outcome
  if(outcome=="heart attack")
  {
    column<-11
  }
  else if(outcome=="heart failure")
  {
    column<-17
  }
  else if(outcome== "pneumonia")
  {
    column<-23
  }
  else
  {
    stop("invalid outcome")
  }
   #get records with that state
  state_records<-subset(outcome_data,outcome_data$State==state)
  #set the column records as numeric from character
  state_records[,column]<-as.numeric(state_records[,column])
  #get the minimum mortality rate by column above
  min_mortality<-min(state_records[column],na.rm = TRUE)
  #get hospitalname/names with the minimum mortality
  hospitalname<-subset(state_records,state_records[,column]==min_mortality)
  #sort hospital name by alphabets
  hospitalname<-hospitalname[order(hospitalname$Hospital.Name),]
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  return(hospitalname$Hospital.Name[1])
}


