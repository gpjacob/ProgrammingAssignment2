## This code takes advantage of the scoping rules of the R language 
## to create an efficient matrix invertor for different invertible matrices


## This function will store a list of functions that 
## 1. Get: Returns the matrix passed as input
## 2. Set: Change the input matrix
## 3. setinverse: stores the value of the inverse matrix
## 4. getinverse: returns value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function checks against memory if the inverse is previously stored and if so
## returns is otherwise it calculates the inverse and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

c=rbind(c(1, -1/4), c(-1/4, 1))
g<-makeCacheMatrix(c)
cacheSolve(g)

d<-rbind(c(4,3), c(3,2))

h<-makeCacheMatrix(d)
cacheSolve(h)
cacheSolve(g)

