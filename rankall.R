rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  
  ## Read outcome data
  outcome_data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  column<-numeric()

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
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcome_data[,column]<-as.numeric(outcome_data[,column])
  rank_num=numeric()
  #sort the records in descending order of state first and then ascending order of mortality rate and hospital name in  that order
  outcome_data<-outcome_data[order(outcome_data[,7],outcome_data[,column],outcome_data[,2],decreasing = FALSE, na.last=NA),]
  
  if(num=="best")
  {
    rank_num<-1
  }
  else if (num=="worst")
  {
    rank_num<-1
    outcome_data<-outcome_data[order(outcome_data[,7],-outcome_data[,column],outcome_data[,2],decreasing = FALSE, na.last=NA),]
  }
  else if(is.na(as.numeric(num)))
  {
    stop("invalid number")
  }
  else if(!is.na(as.numeric(num)))
  {
    num<-as.numeric(num)
 
    rank_num<-num;
    
  }
  #outcome_data$Hospital.Name
  
  state_data<- split(outcome_data,outcome_data$State)
  
  #get all states
  all_states<-unique(outcome_data$State)
  
  #create new data frame to store data
  return_data<-matrix(,nrow = length(all_states),ncol = 2)
  colnames(return_data)<-c("hospital","state")
  i<-1
  #loop through the list and return data frame
  for(letter in all_states)
  {
    #return_data[i,][1]<-letter
    #return_data[i,][2]<-state_data[[letter]][2][rank_num,]
    return_data[i,]<-c((state_data[[letter]][2][rank_num,]),letter)
    i<-i+1
  }
  colnames(return_data)<-c("hospital","state")
  as.data.frame(return_data) 
  
}