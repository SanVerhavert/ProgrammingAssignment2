################################################################################
#                                                                              #
#        Programmed by San Verhavert                                           #
#                                                                              #
#        Description: These functions calculate and cache the inverse of a     #
#                     matrix or retreive the cached sollution and              #
#                     return the answer                                        #
#                                                                              #
################################################################################

################################################################################
#makeCacheMatrix calculates the inverse of matrix x and caches it and          #
#   returns it.                                                                #
################################################################################


makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  
  set <- function(y) #   set the value of the matrix
  {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x#   get the value of the matrix
  
  setInv <- function(inv)#   set the value of the inverse
  {
    s <<- inv
  }
  
  getInv <- function() s#   get the value of the inverse

  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


################################################################################
#cacheSolve checks if there exists a cashed inverse of matrix x.               #
#     If yes: retreives the cashed sollution                                   #
#     If no: calls cacheSolve                                                  #
################################################################################

cacheSolve <- function(x, ...) {
  inv <- x$getInv()#    try to get the cached inverse
  
  #   check if there is an inverse in cache (if the returned object is not null)
  if(!is.null(inv))
  {
    message("Getting cached data!")
    return(inv)
  }
  
  matr <- x$get()#    get the cached matrix x
  
  inv <- solve(matr, ...)#    calculate the inverse of matrix x
  
  x$setInv(inv)#    set the inverse matrix in cache
  
  inv#    return the inverse
}
