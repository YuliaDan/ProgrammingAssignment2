## The two functions below create a matrix object, calculate its inverse, or, if the inverse has 
## already been calculated, retreve the inverse matrix from the cache

## The first function returns the matrix x, changes it, stores the value of the inverse in variable m, ## and gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set = set, get = get, 
			setinverse = setinverse, getinverse = getinverse)		
}


## The second function calculates the inverse of the matrix created with makeCacheMatrix, or, if the inverse has already been calculated, retreaves it from the cache instead of calculating it again. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
        	message ("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
