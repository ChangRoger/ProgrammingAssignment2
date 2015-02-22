## makeCahceMatrix will take a matrix argument x and store this matrix.
## Then by using cacheSolve, the inverse of the matrix will be calculated with solve()
## If say this inverse was just calculated by cacheSolve, we can quickly return the value
## instead of recalculating it. This greatly cuts down computation time for large matrices.

## makeCacheMatrix will empty out all required parameters, then each child function
## will do its job. set changes the matrix used. get will print out the current matrix cached.
## setinverse will assign a value as the inverse of the matrix. getinverse returns the value
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i<<-matrix()
  set <- function(y = matrix()){
    x<<-y
    i<<-matrix()
  }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve will check for the variable i being populated with the inverse matrix or not.
## if no inverse has been set then the function will go on and compute the inverse of the
## given cached matrix x. If the same cached matrix's inverse is to be solved again, the 
## function will just return the value of its cached inverse and end the function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.na(i[1])) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()  
  i <- solve(data)
  x$setinverse(i)
  i     
}