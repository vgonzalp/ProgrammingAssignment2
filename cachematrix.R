## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function contains 4 functions stored in a list.
##The first ('set') allows to define a matrix and assigns the value "NULL" to its inverse (m_inverse).
##The second ('get') returns the stored matrix.
## the third ('setsolve') assigns the value passed as a parameter to the inverse of the matrix
##(regardless which matrix is already stored).
## the last one ('getsolve')returns the stored value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inversa <- NULL
  set <- function(y) {
    x <<- y
    m_inversa <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m_inversa <<- solve
  getsolve <- function() m_inversa
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inversa <- x$getsolve()
  ##verifies that the value of the inverse matrix does not exist,
  ##if it exists, it takes the stored value and returns it in m_inverse,
  ##otherwise it calculates the inverse of the matrix taking the matrix
  ##using the 'x$get()' function and returns the result
  ##in m_inverse and stores the value in the list using 'x$setsolve' function
  if(!is.null(m_inversa)) {
    message("getting cached data")
    return(m_inversa)
  }
  data <- x$get()
  m_inversa <- solve(data, ...)
  x$setsolve(m_inversa)
  m_inversa
}
