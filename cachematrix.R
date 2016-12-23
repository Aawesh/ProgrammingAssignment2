#12/22/2016

#This program will store a inverse of a matrix if it is already calculated
#so that when we want to calculate the inverse of the same matrix that has already been
#calculated previously, it won't recalculate
#Instead, it will get from the cache
#Otherwise, it will calculate and again store in in cache

#This function will store the matrix and provides methods to get,set the matrix itself and the inverse of it

makeCacheMatrix <- function(x = matrix()){
  inverse<<-NULL
  
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get <- function(){
    return(x)
  }
  
  setInverse <- function(){
    inverse<<-solve(x)
  }
  
  getInverse <- function(){
    return(inverse)
  }
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) #returns a special vector
}


#This function first searches in the cache, of found displays the value,
#otherwise calculate the inverse and then saves into the cache for later use
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  newInverse <- solve(data)
  x$setInverse()
  return(newInverse)
}
