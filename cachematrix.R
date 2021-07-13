## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #Initializing inverse as Null
  set<-function(y){
    x <<- y
    inv<<-NULL
  }
  get<-function() x   #function to get a Matrix
  setInverse<-function(inverse) inv<<-NULL
  getInverse<-function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getInverse() #Checking whether the Inverse is Null
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv) #Return the Inverse Value
    }
    mat <- x$get()
    inv <- solve(mat, ...) #Calculates the Inverse Value
    x$setInverse(inv)
    inv
  }

