## wrap a normal matrix so that cache inverve can 
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(.mat = matrix()) {
    .inv <- NULL
    set <- function(mat) {
        .mat <<- mat
        .inv <<- NULL
    }
    get <- function() .mat
    setinv <- function(inv) .inv <<- inv
    getinv <- function() .inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function
cacheSolve <- function(mat, ...) {
    inv <- mat$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setinv(inv)
    inv
}

## Test using Hilbert matrix
## note that Hilbert matrices are famous for ill condition
hilbert <- function(n) { 
    i <- 1:n
    1 / outer(i - 1, i, "+") 
}
x <- hilbert(8);
y <- makeCacheMatrix(x)
for (i in 1:10) {
	i
    cacheSolve(y)
}
