## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix function takes an invertible matrix as an input and 
## returns a list - special "matrix" object that can cache its inverse. 


## cacheSolve computes the inverse of the matrix, stores it and returns it. 
## if the inverse was computed then it returns it from storage

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL 		##initialize m to NULL

      set <- function(y) {	## function which sets the matrix x to new matrix y
            x <<- y
            m <<- NULL
      }

 
      get <- function() x 	## returns matrix x 
      setinv <- function(inv) m <<- inv ## sets value of m to inv
      getinv <- function() m 		## returns value of m
      list(set = set, get = get, 	## returns the list with 4 elements named set, get, setinv and getinv
           setinv = setinv,
           getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

      inv <- x$getinv() 	## return value of getinv element in list from makeCacheMatrix which is the value of variable m in that function
      if(!is.null(inv)) {	## check if the returned value is not NULL, if true return value of m
            message("getting cached data")
            return(inv)
      }
      data <- x$get()		## set data to the value of get element in the list, i.e. to the initial matrix
      inv <- solve(data, ...)	## invert the matrix
      x$setinv(inv)		## set value of m to the inverse matrix 
      inv			## return inverse matrix

}