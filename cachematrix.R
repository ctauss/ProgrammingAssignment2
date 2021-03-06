###############################################################################################
# This version uses names that, hopefully, makes the logic of the program self-documenting.   #
#                                                                                             #
# makeCacheMatrix - is a function that creates a vector of functions which produces           #
# a vector of 4 functions: 2 each to get and set the Matrix itself  and its inverse.          #
# These are stored in variables that create a function closure.                               #
###############################################################################################

makeCacheMatrix <- function(theMatrix = matrix()) {
	invMatrix <- NULL

	# Set matrix to closure variable.  
	# Note that this sets to NULL any previously cached inverted matrix.
      setMatrix <- function(y) {
      	theMatrix <<- y
            invMatrix <<- NULL
  	}
      
	# Get matrix from closure variable
	getMatrix <- function(){
		theMatrix
	}
	
	# Set inverted matrix to closure variable
	setInvMatrix <- function(theInvMatrix){
		invMatrix <<- theInvMatrix
	}
    	
	# Get inverted matrix from closure variable 
	# Note that if this is not set, the function returns NULL
	getInvMatrix <- function(){
		invMatrix
	}

	# This is the return value which returns a vector of callable functions which can refer to the closure variables,
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## cacheSolve - is a function which returns a matrix that is the inverse of the Matrix was set by the setMatrix() function.
## First it checks if the inverse is already in the cache.  If so, it uses that - if not it calculates the inverse matrix
## and places it in the cache before returning it.

cacheSolve <- function(x, ...) {
	# This gets the inverted matrix from the cache to a local function variable
	InvMatrix <- x$getInvMatrix()
        
	# If the cache value is already set (ie. not NULL), then return that, othewise ...
	if(!is.null(InvMatrix)) {
		message("Getting cached Inversed Matrix")
      	return(InvMatrix)
 	}
      
	# ... if here, this means the cached inverted Matrix is NULL so we need to solve for it ourselves.
	Matrix <- x$getMatrix()
      
	# Compute inverse Matrix and store in a local function variable.
	InvMatrix <- solve(Matrix)
      
	# Set computed inverse Matrix to the cache (i.e. set to the closure variable)
	x$setInvMatrix(InvMatrix)
      
	# Return inverted Matrix.
	InvMatrix
}
