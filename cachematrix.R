## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) 
{
        ##creates a special 'matrix' object that can cache its inverse
        s <- NULL  #set an empty inverse matrix
        set <- function(y) 
        {
                ##sets the value of the matrix (x==y) to the parent environment
                x <<- y
                ##set an empty inverse matrix in the parent environment
                s <<- NULL 
        }
        #gets the value of the matrix
        get <- function() x
        setsolve <- function(solve) s <<- solve
        ##sets the value of the inverse matrix (m) in the parent environment
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #gets the value of the inverse matrix
        s <- x$getsolve()  
        if(!is.null(s)) 
        {
                ## if m is already calculated
                message("Getting inversed matrix.")
                return(s)
        }
        ## if s is not calculated then, calculate the inverse of the matrix
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        #return the inverse matrix
        s
}
