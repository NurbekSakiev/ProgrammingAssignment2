
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	set_Inversed <- function(inversed) inv <<- inversed
	get_Inversed <- function() inv
	list(set = set, get = get, set_Invsersed = setInversed, get_Inversed = get_Inversed)

}


## This function computes the inverse of the special "matrix returned by makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
		inv <- x$get_Inversed()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$set_Inversed(inv)
		inv

}

