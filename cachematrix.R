## makeCacheMatrix(x) would return a list of function which may store two matrices
## setmt is supposed to store some particular matrix, getmt is supposed to get it
## setinvmt is supposed to store the inverse matrix, getinvmt is supposed to get it
## it does NOT calculate inverse matrix automatically

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmt <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmt <- function() x
        setinvmt <- function (inv) i <<- inv
        getinvmt <- function() i
        list(setmt = setmt, getmt = getmt, setinvmt = setinvmt, getinvmt = getinvmt)
        
}


## cacheSolve(x) is supposed to be used on the list created above
## If you already set inverse matrix in that list, it would return that value
## If not, it would calculate the invers matrix of the particular matrix you set in the list

cacheSolve <- function(x, ...) {
        i <- x$getinvmt()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getmt()
        i <- solve(data)
        i
}
