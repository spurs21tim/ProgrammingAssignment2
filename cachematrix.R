## The aim is to save computation time in solving the inverse of a square matrix. 
## "makeCacheMatrix" function creates an special object to store the inverse matrix
## while "cacheSolve" function check if the inverse has been already stored in cache,
## otherwise, calculates it ans stores it (into cache).

## Creation of inverse matrtix object
makeCacheMatrix <- function(x = matrix()) {
     m_inv <- NULL                                ## Inverse matrix is undefined
     set <- function(y) {                         ## Sets the matrix 
          x <<- y
          m_inv <<- NULL
     }
     get <- function() x                          ## Gets the matrix
     set_inv <- function(solve) m_inv <<- solve   ## Sets the inverse matrix          
     get_inv <- function() m_inv                  ## Gets the inverse value           
     list(set = set, get = get,                   ## Creates the special object to store the matrix and the inverse 
          set_inv = set_inv,
          get_inv = get_inv)
}

## Recover the inverse value from cache if the matrix x does not change or calculates 
## and stores the inverse
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m_inv <- x$get_inv()                    ## If matrix x does not changes
     if(!is.null(m_inv)) {
          message("getting cached data")
          return(m_inv)                      ## Returns the inverse from Cache in m_inv exists
     }
     data <- x$get()                         ## Gets the new matrix x
     m_inv <- solve(data, ...)               ## Calculates the inverse
     x$set_inv(m_inv)                        ## Stores the inverse in inverse object
     m_inv                                   ## Returns the inverse matrix
}

## Example how to use both functions:
## > A<- makeCacheMatrix(x)
## > cacheSolve(A)