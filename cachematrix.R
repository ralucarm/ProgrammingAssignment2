# Calculate the Inverse of a matrix -> normal way
matrix <- matrix(1:4, nrow = 2, ncol=2)
matrix
matrix_inv <- solve(matrix) 
matrix_inv

# Calculate the inverse of a matrix -> caching it
# The function "makeCacheMatrix", makeVector creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the solve function (the inverse of the matrix)
# get the value of the solve function (the inverse of the matrix)
makeCacheMatrix <- function(mat = matrix) {
  m <- NULL
  set <- function(y) {
    mat <<- y
    m <<- NULL
  }
  get <- function() mat
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Running it:
matrix <- matrix(1:4, nrow = 2, ncol=2)
my_mat <- makeCacheMatrix(matrix)
my_mat

# The function "cacheSolve" calculates the inverse of the matrix (list) created with the above function. 
# If the inverse has already been calculated, it will get it from the cache and not calculate it again. (In the case of the same object being used)
cacheSolve <- function(mat, ...) {
  m <- mat$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- mat$get()
  m <- solve(data, ...)
  mat$setinv(m)
  m
}

# Running it:
my_mat
cacheSolve(my_mat)
cacheSolve(my_mat)


