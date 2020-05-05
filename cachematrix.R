## The purpose of this script is to store a cached copy of the inverse
## of a matrix and return this cached copy, as long as the original has
## not change. As matrix inversion is a computationally intense process,
## this script allows the user to access the matrix inverse without having
## to calculate it every time.

## The makeCacheMatrix function takes an argument x, (note: x must be an
## invertible matrix otherwise solve() will not work later on) and creates an
## R object which stores x and its inverse. The matrix can be changed using the
## set function, which will update the cached matrix, and clear the cached 
## matrix inverse (ie. i <<- NULL), to ensure when cacheSolve is called, it
## does not return the inverse that was calculated for a previous matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function (inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function takes as its argument the R object created by
## makeCacheMatrix, then either returns the previously calculated matrix
## inverse, or if the inverse has not been calculated or the original 
## matrix has been changed, the inverse is calculated and the R object is 
## updated with the newly calculated inverse.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
