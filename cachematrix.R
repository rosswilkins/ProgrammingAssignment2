##The functions allow you to create a matrix object, to compute its inverse 
##matrix and store the result in the cache, so that the inverse matrix does not 
##have to be computed again and again.


## makeCacheMatrix creates a special "matrix" object,
## which is really a list containing a function to
## 1.set the value of the matrix  (set)
## 2.get the value of the matrix  (get)
## 3.set the value of the inverse matrix (getmInv)
## 4.get the value of the inverse matrix (setmInv)


makeCacheMatrix <- function(x = matrix()) {
      mInv <- NULL
      set <- function(y) {
            x <<- y
            mInv <<- NULL
      }
      
      get <- function() x
      setmInv <- function(InvMat) mInv <<- InvMat
      getmInv <- function() mInv
      list(set = set, get = get,
           setmInv = setmInv,
           getmInv = getmInv)
}


## cacheSolve calculates the inverse matrix of the special "matrix" created
## with the above function. 
## However, it first checks to see if the inverse matrix has already been 
## calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of
## the inverse matrix in the cache via the setmInv function.

cacheSolve <- function(x, ...) {
      mInv <- x$getmInv()
      if(!is.null(mInv)) {
            message("getting cached data")
            return(mInv)
      }
      data <- x$get()
      mInv <- solve(data, ...)
      x$setmInv(mInv)
      mInv
    
}
