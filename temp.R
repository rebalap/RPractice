inversemat <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  matrix_1<-x
  i<-dim(matrix_1)[1]
  j<-dim(matrix_1)[2]
  inverse<-matrix(nrow = j,ncol = i)

  for(row in 1:i)
  {
    inverse[,row]<-matrix_1[row,]
  }
  inverse
  
}