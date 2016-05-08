## Anne Leroy - May 8th 2016
##
#  Inversing a huge matrix can be time consuming
#  It's interesting to compute this matrix only once
#  and retrieve it's value from cache 


# 
######  Please try (for example) : 
#
# matrice <- makeCacheMatrix(matrix(c(1, 0, 0,1), c(2, 2)))
#
# cacheSolve(matrice)
#
## You get : 
##             [,1] [,2]
##       [1,]    1    0
##       [2,]    0    1
#
# cacheSolve(matrice)
#
## You get :
##  Retrieve from cached data
##             [,1] [,2]
##       [1,]    1    0
##       [2,]    0    1



 # This function creates the matrice and initialize cache to NULL
 
 makeCacheMatrix <- function(x = matrix()) {
         cache <- NULL
         set <- function(y) {
                 x <<- y
                 cache <<- NULL
         }
         get <- function() x
         setinverse <- function(inv) cache <<- inv
         getinverse <- function() cache
         list(
                 set = set,
                 get = get,
                 setinverse = setinverse,
                 getinverse = getinverse
         )
 }
 
 

 
# 
# This function inverse the matrix or retrieve the data from cache
# 
 cacheSolve <- function(x, ...) {
         cache <- x$getinverse()
         if(!is.null(cache)) {
                 message("Retrieve from cached data")
                 return(cache)
         }
         matrice <- x$get()
         cache <- solve(matrice, ...)
         x$setinverse(cache)
         cache
 }
 
 
 # matrice <- makeCacheMatrix(matrix(c(1, 0, 0,1), c(2, 2)))
 # cacheSolve(matrice)
 # cacheSolve(matrice)

 
 
 
