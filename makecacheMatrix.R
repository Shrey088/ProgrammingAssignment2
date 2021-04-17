#making cache matrix  
makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
#set the value of matrix using another function       
      set <- function(y){
#double arrow assignment operator used for conjunction with closure 
          x <<- y
          inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
#get the value of the inverse 
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }

#getting the value of cache matrix 
cacheSolve <- function(x, ...){
          inv <- x$getInverse()
          if(!is.null (inv)){
              message("getting cached data")
              return(inv)
          }
          mat <- x$get()
          inv <- solve(mat, ...)
#setting the value of cache 
          x$setInverse(inv)
          inv
 }