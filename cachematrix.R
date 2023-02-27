
	## Objective is to write a pair of functions, namely, "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
	## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse for the input (which is assumed to be an invertible square matrix for this program assessment)
	makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
	  inv <- NULL
	  set <- function(y) {
	    x <<- y
	    s <<- NULL
	  }
	  get <- function() x
	  setinv <- function(inverse) s <<- inverse
	  getinv <- function() inv
	  list(set = set, get = get,
	       setinv = setinv,
	       getinv = getinv)
	}
	##
	## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above only if the inverse has already been calculated 
	cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
	  s <- x$getinv()
	  if(!is.null(s)) {
	    message("getting cached result")
	    return(s)
	  }
	  data <- x$get()
	  s <- solve(data, ...)
	  x$setinv(inv)
	  Inv
	}

