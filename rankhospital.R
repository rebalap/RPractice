
#ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  column<-numeric()
  ## Check that state and outcome are valid
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
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  #get records with that state
  state_records<-subset(outcome_data,outcome_data$State==state)
  #set the column records as numeric from character
  state_records[,column]<-as.numeric(state_records[,column])
  rank_num=numeric()
  #sort the records in descending order of mortality rate
  state_records<-state_records[order(state_records[,column],state_records[,2],decreasing = FALSE, na.last=NA),]
  
  if(num=="best")
  {
    rank_num<-1
  }
  else if (num=="worst")
  {
    rank_num<-1
    state_records<-state_records[order(state_records[,column],state_records[,2],decreasing = TRUE, na.last=NA),]
  }
  else if(is.na(as.numeric(num)))
  {
    stop("invalid number")
  }
  else if(!is.na(as.numeric(num)))
  {
    num<-as.numeric(num)
    ##if(num>length(state_records[,column]))
    ##{
    ## stop("invalid number")
    ## }
    ##else
    ##{
    rank_num<-num;
    ##}
  }
  #resort the records based on hospital name for rank
  #state_records<-state_records[order(state_records$Hospital.Name,decreasing = FALSE, na.last=NA),]
  state_records$Hospital.Name[rank_num]
}