makeVector<- function(x=numeric())
{
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<- function()x
  
  setmean<- function(mean)m<<-mean
  
  getmean<-function()m
  
  list(set=set,get=get,setmean=setmean,getmean=getmean)
}

cachMean<-function(x,...){
  m<-x$getmean()
  if(!is.null(m))
  {
    message("getting cached data")
    return (m)
  }
  data<-x$get()
  m<- mean(data,...)
  x$setmean(m)
  m

}


#programming assignment

#setting set get functions for matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  ##create inverse matrix variable
  invmatrix<-NULL
  
  setmatrix<- function(y){
    x<<-y
    invmatrix<<-NULL
  }
  ##get matrix function
  getmatrix<-function() x
  
  ##set inverse matrix function
  setmatrixinverse<-function(mat_rix) invmatrix<<-mat_rix
  
  ##get inverse matrix function
  getmatrixinverse<-function()invmatrix
  
  #create list of functions for get and set
  list(setmatrix=setmatrix,getmatrix=getmatrix,setmatrixinverse=setmatrixinverse,getmatrixinverse=getmatrixinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix<-x$getmatrixinverse()
  ## check if the inverse is cached 
  if(!is.null(invmatrix))
  {
    message("getting cached data")
    return(invmatrix)
  }
  ##if theres no cached data execute solve function to get the inverse
  data<-x$getmatrix()
  invmatrix<-solve(data,...)
  x$setmatrixinverse(invmatrix)
  invmatrix
  
}