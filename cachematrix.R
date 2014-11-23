## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix returns a list of 4 functions - to se a matric, get a matric, set its inverse and get its inverse
## cacheSolve fetches the inverse of the matrix passed as parameter if it has been cached. Else, it calculates the inverse and caches it
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv_x<-NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)
        inv_x
}
