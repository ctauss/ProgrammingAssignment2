## This version uses names that, hopefully, makes the logic of the program self-documenting. 
## 
## makeCacheMatrix - is a function that creates a vector of functions which produces 
## a vector of 4 functions: 2 each to get and set the Matrix itself  and its inverse.  
## These are stored in variables that create a function closure.

makeCacheMatrix <- function(theMatrix = matrix()) {
	invMatrix <- NULL

      setMatrix <- function(y) {
      	theMatrix <<- y
            invMatrix <<- NULL
  	}
      
	getMatrix <- function(){
		theMatrix
	}

	setInvMatrix <- function(theInvMatrix){
		invMatrix <<- theInvMatrix
	}
    		
	getInvMatrix <- function(){
		invMatrix
	}
  	
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## cacheSolve - is a function which returns a matrix that is the inverse of 'theMatrix' 
##(which was set by the setMatrix() function).
## But first it checks if the inverse is already in the cache.  
## If so, it uses that - if not it calculates the inverse and places it in the cache.

cacheSolve <- function(x, ...) {
	InvMatrix <- x$getInvMatrix()
        
	if(!is.null(InvMatrix)) {
		message("Getting cached Inversed Matrix")
      	return(InvMatrix)
 	}
        
	Matrix <- x$getMatrix()
        
	InvMatrix <- solve(Matrix)
        
	x$setInvMatrix(InvMatrix)
        
	InvMatrix
}
