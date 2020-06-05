## this function create a matrix that can catch its inverse


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ## the set function set the value of the matrix
      set <- function(y){
          y <<- x
          inv <<- NULL
      }
      ## get the value of the matrix
      get <- function() x
      ## this function set the value of the inverse 
      setinv <- function(y) inv <<- y
      ## this function get the value of the inverse 
      getinv <- function() inv
      
      list(set = set , get = get , setinv = setinv , getinv = getinv)
}


## this function return the inverse of the matrix
## if the inverse has not been calculated yet, it calculates, it set it, 
##then returns its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##2 first we get the actual value of the inv
        inv <- x$getinv()
        ## if the inverse is not NULL
        if(!is.null(inv)){
          ## we return the inv if it's not null
          return(inv)
          
        }
        ##we calculate the inverse then we return its value
        inv <- solve(x$get())
        x$setinv(inv)
        inv
}

