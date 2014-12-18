# makeCacheMatrix() acts as a storage container that takes a matrix and returns a list of functions pertaining to that matrix.
# cacheSolve() attempts to retrieve the inverted matrix by executing "getinv()" against our container object; if that expression 
# evaluates to NULL, we proceed with the computationally expensive 'solve' but then store the answer back in our container for future use

## makeCacheMatrix() takes a single matrix argument and returns a list of functions (a "cache" object that exposes 4 functions)
## set, get, setinv, and getinv
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL				#initialize variable i, which will be our inverted matrix
	set <- function(y) {	#same as if we just called makeCacheMatrix
		x <<- y
		i <<- NULL
	}
	get <- function() x		#returns the original matrix
	setinv <- function(inv) i <<- inv	#takes the external argument passed and assigns to variable i
	getinv <- function() i				#returns variable i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes a list argument created from cacheMatrix() and returns an inverted matrix
cacheSolve <- function(x, ...) {
	i <- x$getinv()			#whatever is stored in the cache, we get it here using 'getinv()'
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	} 	
#if the cache is empty, then execute these next lines	
	data <- x$get()			#get the original matrix stored in the cache
	i <- solve(data, ...) 	#invert the matrix using 'solve' builtin
	x$setinv(i) 			#store the answer for future use
	i						#return the answer to the caller
}
