## Put comments here that give an overall description of what your
## functions do

## 'cacheSolve' is a client function that uses 'makeCacheMatrix' function in its implementation.
## The input is expecting a "special matrix" made from 'makeCacheMatrix'. 
## The output is the inverse matrix coming whether from the "special matrix"'s cache or computation.

## Write a short comment describing this function

## The fist function 'makeCacheMatrix' takes an argument x, a matrix which we want to invert. 
## It creates a special object - a list containing four functions:
## * set
## * get
## * setInverse
## * getInverse
## 'makeCacheMatrix' can cache x and inverse of x matrix(if exists and if exists in this object, 
##  otherwise m == NULL) and return it lately by getInverse()  

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(foo) m <<- foo
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## 'cacheSolve' function takes an argument x of type list made earlier by 'makeCacheMatrix' from matrix which was
## an input for 'makeCacheMatrix'. 
## 
## The output of 'cacheSolve' is the inverse matrix coming whether from the 'makeCacheMatrix' list:
## x$getInverse()  or computation: 
## 		  data <- x$get()          
##		  m <- solve(data)       
##		  x$setInverse(m).

cacheSolve <- function(x) {
        
         m <- x$getInverse()             
		 if(!is.null(m)) {          
			message("getting cached data") 
			return(m)                
		  }
		  data <- x$get()             
		  m <- solve(data)        
		  x$setInverse(m)               
		  m                          
        
}

