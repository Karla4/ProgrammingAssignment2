## Put comments here that give an overall description of what your
## functions do


#1.  `makeCacheMatrix`: This function creates a special "matrix" object
#that can cache its inverse.
#2.  `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.


## Write a short comment describing this function
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the matrix inverse
#4.  get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(inversa) minv <<- inversa
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## Write a short comment describing this function
## RETURNS THE INVERSE OF THE MATRIX

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
    minv <- x$getminv()
    if(!is.null(minv)) {
      message("getting cached data")
      return(minv)
    }
    data <- x$get()
    minv <- solve(data)
    x$setminv(minv)
    minv
}

#SAMPLE RUN:

#> x=rbind(c(1,3,3),c(1,4,3),c(1,3,4))
#> m=makeCacheMatrix(x)
#> m$get()
#> cacheSolve(m)
#  [,1] [,2] [,3]
#  [1,]    7   -3   -3
#  [2,]   -1    1    0
#  [3,]   -1    0    1

