##set Matrix

makeCacheMatrix <- function(x = matrix()) {
  #var ans will be the inverse value
  ans <- NULL #sets value to null
  
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  #get matrix
  get <- function()x
  #set the inverse function
  setans <- function(inverse) ans <<- inverse
  #getting the inverse 
  getans <- function() ans 
  list(set = set, get = get, 
       setans = setans, 
       getans = getans)
}

#Getting the inverse of the original matrix inptu in 'x'
cacheSolve <- function(x, ...) {
  ans <- x$getans()
  #if inverse has already been calculated
  if(!is.null(ans)){
    #skips the calculation by getting from cache
    message("getting cached data")
    return(ans)
  }
  #if not cached it calculates the inverse
  mat <- x$get()
  ans <- solve(mat, ...)
  #sets the value of the inverse in the cache
  x$setans(ans)
  return(ans)
  
}