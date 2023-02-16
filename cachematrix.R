# MAHONK89   COURSERA  R-PROGRAMMING    WEEK 3 ASSIGNMENT ON LEXICAL SCOPING

## I piggybacked off of the makeVector() and cachemean functions provided for examples
## on this assignment. After reading an additional article (link at bottom), the structure,
## design and output of these functions all seemed much more obvious and could be tweaked
## to be used for matrices and calculating/caching the inverse. 

## makeCacheMatrix will assign a group of functions to an assigned object and allow
## for subsetting the object using the nested functions within makeCacheMatrix to get()
## the original input matrix, set() a new matrix, and getinv() to pull the inverse. Must be
## used in conjunction with cacheSolve(). Although setinv() is defined in makeCacheMatrix
## it should not be used on it's own as it can set any value to the inverse. cacheSolve
## and $getinv() will both return that erroneous value since !is.null(i)=TRUE.


## The below function should be assigned any name, myMatrix for example. First the function will
## check the validity of the input - if it is not a square matrix, or matrix at all, the function
## will not run. If the input is a matrix, then
## object i is cleared to negate errors in getinv()
## 4 functions are defined: set(y), get(), setinv(inverse), and getinv()
## Once an object is assigned to makeCacheMatrix, these functions can be called within
## a subset of that object i.e. myMatrix$get() will run the get function.
## myMatrix$set(y) will assign a new matrix to x, it is assigned to the parent function, not within set()
## myMatrix$set(y) also clears value of i since the inverse should be recalculated
## myMatrix$get() returns the original input x which was assigned to the parent function
## myMatrix$setinv(inverse) is defined within makeCacheMatrix but should not be used outside of 
## the function, only to be used automatically by cachsolve. i is the "inverse" variable
## myMatrix$getinv() retrieves the inverse matrix if it has been calculated and cached
## the list is created to assign names to each function to allow for subsetting w/ $ to run

makeCacheMatrix <- function(x = matrix()) {
  stopifnot("Input is not a matrix: Must be a square matrix where nrow(x)=ncol(x)" = TRUE == is.matrix(x),
            "Matrix must be a square where nrow(x)=ncol(x)" = nrow(x)==ncol(x))
  i <- NULL
  set <- function(y) {
    stopifnot("Input is not a matrix: Must be a square matrix where nrow(x)=ncol(x)" = TRUE == is.matrix(y),
              "Matrix must be a square where nrow(x)=ncol(x)" = nrow(y)==ncol(y))
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve only works when the input x was assigned to makeCacheMatrix
## i is checked by pulling the value from the input - if the value is NOT NULL
## then the assigned value is returned. Otherwise, cacheSolve pulls the original
## matrix from x and assigns i the value of the inverse = solve(matrix) by using
## the $setinv(inverse=i) function through makeCacheMatrix, then returns i. 
## if the same function is run again on the same variable, the calculation is not 
## ran, but instead the inverse is pulled from the cached value i. 

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix)
  x$setinv(i)
  i
}


## examples for testing
## typed up for easy input, feel free to use your inputs

# rsm makes a random 5x5 matrix
rsm<-matrix(sample(-5:5,25,replace = T),nrow = 5,ncol = 5)
# assigns to myMatrix using the 5x5
myMatrix<-makeCacheMatrix(rsm) 
myMatrix # run to see the list output of functions (will not return matrix rsm)


myMatrix$get() # run to see the original matrix, rsm
cacheSolve(myMatrix) # will pull from cache/solve the inverse and return it
myMatrix$getinv() # return inverse if calculated, null otherwise

# rerun the rsm assignment and run below to assign a new matrix, run the above 3 in any order
rsm2<-matrix(sample(-5:5,36,replace = T),nrow = 6,ncol = 6)
# can be used to change the input matrix, same rules apply (must be square matrix)
myMatrix$set(rsm2)  


round(myMatrix$get() %*% myMatrix$getinv(),14) 
    # running matrix multiplication on the original
    # matrix and the calculated inverse will return the identity matrix. Rounding to 14 decimal
    # places as typically the results are to the -15 power or smaller.

 # WILL NOT WORK
makeCacheMatrix(1:10) #inputting a vector stops the function and returns an error
makeCacheMatrix(matrix(1:10,nrow=5)) #inputting a non-square matrix will stop the function
    # and return an error
myMatrix$set(matrix(1:10,nrow=5)) 
cacheSolve(matrix(1:16,nrow=4)) #will not work on a matrix alone - must be used on 
# the object assigned to the makeCacheMatrix function as that's where the nested functions
# are defined and contained.  
myMatrix$get() # returns original matrix


# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
# this article in the discussion forum for this project really helped break down the makeVector function
# so that each part was easily understandable. Prior to reading this I was unaware of using nested
# functions via subsetting an object assigned to the main function, as well as the usage of "<<-"