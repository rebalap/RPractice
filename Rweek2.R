pollutantmean<- function (directory,pollutant, id=1:332)
{
  #set directory
  

  #get file list
  filelist<- list.files(directory)
  
  #set file paths
  filepaths<- paste(directory,"/",filelist,sep = "")
  
  currentfile<-  data.frame()
  nonNAvector<-c()
  #browse through files
  for (i in id){
    currentfile<-read.csv(filepaths[i])
    #get non NA values
    nonNAcurrentfile<- currentfile[!is.na(currentfile[,pollutant]),pollutant]
    nonNAvector<- c(nonNAvector,nonNAcurrentfile)
    
  }
  result<-mean(nonNAvector)
  return(result)
  
}

##Question 2

complete <- function(directory, id= 1:332)
{
  #get file list
  filelist<- list.files(directory)
  
  #set file paths
  filepaths<- paste(directory,"/",filelist,sep = "")
  
  #initialize data frame
  newdataframe <- data.frame(id,nobs=0)
  names(newdataframe)<- c("id","nobs")
  num<- integer()
  num<-1 
  for (i in id){
    currentfile<-read.csv(filepaths[i])
    #get non NA values
    #newdataframe["id","nob"]<- c(id,mean(currentfile))
    #rbind(newdataframe,c(i,sum(complete.cases(currentfile))))
    #nonNAvector<- c(nonNAvector,nonNAcurrentfile)
    newdataframe[num,"nobs"]=sum(complete.cases(currentfile))
    num<- num +1 
  }
  return(newdataframe)
}


#question 3

##Question 2

corr <- function(directory, threshhold=0)
{
  #get file list
  filelist<- list.files(directory)
  
  #set file paths
  filepaths<- paste(directory,"/",filelist,sep = "")
  
  #initialize data frame
  #newdataframe <- data.frame(id,nobs=0)
  #names(newdataframe)<- c("id","nobs")
  #num<- integer()
  #num<-1 
  correlation_vector<- c()
  for (i in 1:length(filelist)){
    currentfile<-read.csv(filepaths[i])
    #get non NA values
    #newdataframe["id","nob"]<- c(id,mean(currentfile))
    #rbind(newdataframe,c(i,sum(complete.cases(currentfile))))
    #nonNAvector<- c(nonNAvector,nonNAcurrentfile)
     if(sum(complete.cases(currentfile))>threshhold)
  {
    correlation_vector <- c(correlation_vector,cor(currentfile[,"nitrate"],currentfile[,"sulfate"],use="complete.obs"))
  }
 
  }
  return(correlation_vector)
  
  
}

