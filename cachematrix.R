## Assignment 2 functions 
## 1st functionCacheMatrix for storing a given matrix X in memory

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory

makeCacheMatrix <- function(X = matrix()) {
  inverse <- NULL
  set <- function(Y){
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve function
## This function calculates the inverse of a given matrix from makeCacheMatrix else if it is already in memory then displays it

## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition

cacheSolve <- function(X, ...) 
{
  if(require("corpcor")){
    print("Corpcor has been correctly loaded")
  } else {
    print("Installing corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed")
    } else {
      stop("corpcor installation unsuccessful")
    }
  }
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("Retriving matrix from memory")
    return(inverse)
  }
  message("Computing inverse")
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  X$setinverse(inverse)
  Inverse
}


