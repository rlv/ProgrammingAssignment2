## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set the value of a matrix, 
#  get the value of it, 
#  set inverse of a matrix and 
#  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set = function(y) {
            x<<- y
            inv <<- NULL
      }
      get =function()x
      setinv=function(inverse) inv<<- inverse
      getinv=function() inv
      list(set=set, get=get,
           setinv=setinv, 
           getinv=getinv)
}


## Write a short comment describing this function
#if the inverse of a matrix is already solved then it grabs its value from cache otherwise it calculates it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv=x$getinv()
# check if inverse is solved  already. If solved, then get it from cache otherwise calculate the inverse      
      if (!is.null(inv)){
            message('getting cached data')
            return(inv)
      }
      
      #0therwise calculate it
      matrix.data=x$get()
      inv=solve(matrix.data, ...)
      
      x$setinv(inv)
      return(inv)
      }
