## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix

## Write a short comment describing this function
##This function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
##get the value of the matrix, set the Inverse Matrix and get the Inverse Matrix. The 
##matrix object can cache its own object. 


makeCacheMatrix <- function(x = matrix()) {
	invMat <- NULL
	
	#set the value of the Matrix 
	setMat <- function (y) {
	x <<- y
	invMat <<- NULL
}

	getMat <- function() x 						#gets the value of the matrix
	setInv <- function(inverse) invMat <<- inverse		#sets the value of the invertible matrix
	getInv <- function() invMat					#get the value of the invertible matrix
	list(setMat = setMat, getMat = getMat,setInv = setInv, getInv = getInv)	 

}
## Write a short comment describing this function

##The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
##input and checks the inverse matrix from makeCacheMatrix(matrix)if it has any value in it or not.
##In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data
##and set the invertible  matrix by using the solve function.In case inverse matrix from makeCacheMatrix((matrix) 
##has some value in it, it returns a message  "Giving Back Cached Inverted Matrix" along with the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	##Get the value of the invertible matrix from the makeCacheMatrix function
	invMat <- x$getInv()
	if(!is.null(invMat)) {						#if inverse matrix is not NULL
		message("Giving Back Cached Inverted Matrix")		#Type message: Giving Back Cached Inverted Matrix 
		return(invMat)							#return the invertible matrix
}

	#if value of the invertible matrix is NULL then
	MatrixData <- x$getMat()                     #get the original Matrix Data 
	invMat <- solve(MatrixData, ...)             #use solve function to inverse the matrix
	x$setInv(invMat)                         #set the invertible matrix 
	 return(invMat)                               #return the invertible matrix
}


