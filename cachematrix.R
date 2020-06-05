## this function create a matrix that can catch its inverse


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
          y <<- x
          inv <<- NULL
      }
      get <- function() x
      setinv <- function(y) inv <<- y
      getinv <- function() inv
      
      list(set = set , get = get , setinv = setinv , getinv = getinv)
}


## this function return the inverse of the matrix
## if the inverse has not been calculated yet, it calculates, it set it, 
##then returns its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(is.null(inv)){
          inv <- solve(x$get())
          x$setmean(inv)
          return(inv)
        }
          inv
}

