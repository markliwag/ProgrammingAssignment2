## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix

## Write a short comment describing this function
##This function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
##get the value of the matrix, set the Inverse Matrix and get the Inverse Matrix. The 
##matrix object can cache its own object. 


makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	
	#set the value of the Matrix 
	setMatrix <- function (y) {
	x <<- y
	invMatrix <<- NULL
}

	getMatrix <- function() x 						#gets the value of the matrix
	setInverse <- function(inverse) invMatrix <<- inverse		#sets the value of the invertible matrix
	getInverse <- function() invMatrix					#get the value of the invertible matrix
	list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse, getInverse = getInverse)	 

}
## Write a short comment describing this function

##The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
##input and checks the inverse matrix from makeCacheMatrix(matrix)if it has any value in it or not.
##In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data
##and set the invertible  matrix by using the solve function.In case inverse matrix from makeCacheMatrix((matrix) 
##has some value in it, it returns a message  "Getting Cached Invertible Matrix" along with the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	##Get the value of the invertible matrix from the makeCacheMatrix function
	invMatrix <- x$getInverse()
	if(!is.null(invMatrix)) {						#if inverse matrix is not NULL
		message("Getting Cached Invertible Matrix")		#Type message: Getting Cached Invertible Matrix 
		return(invMatrix)							#return the invertible matrix
}

	#if value of the invertible matrix is NULL then
	MatrixData <- x$getMatrix()                     #get the original Matrix Data 
	invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
	x$setInverse(invMatrix)                         #set the invertible matrix 
	 return(invMatrix)                               #return the invertible matrix
}


