## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## On creation, stores matrix into mtx, sets up functions to:
## set a matrix as the new matrix. At the same time remove the cached matrix inverse
## get the current matrix
## set the matrix inverse
## get the matrix inverse
makeCacheMatrix <- function(mtx = matrix()) {
	mInv = NULL
	set = function(newMatrix) {
		mtx <<- newMatrix
		mInv <<- NULL
	}
	
	get = function() return mtx
	setInverse = function(inverse) mInv <<- inverse
	getInverse = function() return mInv
	list(set = set, get = get, 
		 setInverse = setInverse, 
		 getInverse = getInverse)
}


## Write a short comment describing this function
## tries to get the matrix inverse of the special matrix class
## if the cache is not empty, return the cached value
## otherwise, get the original matrix, perform the inversion,
## set the inversion, and return it.
cacheSolve <- function(mCM, ...) {
	## Return a matrix that is the inverse of 'x'
	mInv = mCM$getInverse()
	if(!is.null(mInv)) {
		message("getting cached data")
		return(mInv)
	}
	data = mCM$get()
	mInv = solve(data)
	mCM$setInverse(mInv)
	return mInv
}
