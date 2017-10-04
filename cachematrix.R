##Put comments here that give an overall description of what your functions do

## Creates a special matrix which is realy a list contaning a function that will:
## first set the value of the matrix
## second get the value of the matrix
## third set the value of the inverse
## forth get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	## sets the value of m to NULL
	m<-NULL
	## sets the value of the matrix
	set<-function(y){
		## sets the value of x to y (not dependent on current environment)
		x<<-y
		## sets the value of m to NULL (not dependent on current environment)
		## this effectively clears the cache
		m<<-NULL
	}
	## gets the value of the matrix
	get<-function()x
	## sets the value of the inverse
	setinv<-function(inverse) m <<-inverse
	## gets the value of the inverse
	getinv<-function() m
	## creates a list of the four above defined functions
	list(set=set, get=get, setinv=setinv, getinverse=getinv)
}

## Determines if the inverse of the matrix created with makeCacheMatrix has been calculated
## if it has already been calulated it will get the inverse from the cache and skip the compultation
## if it has not been calculated previously it will calculate the inverse 
## the valule for the inverse will then be set in the cache via the setinv function 

cacheSolve <- function(x,...) {
      ## sets the value of m to the value of the inverse
 	m<-x$getinv()
	## determines if the inverse exists in the cache
	if(!is.null(m)) {
		## if m is not null then it exists and this message is printed
		message("getting cached data")
		## returns the cached value for the inverse
		return(m)
	}
 	## sets the value of data to the value of the matrix if the cache does not exist
	data<-x$get()
	## sets the value of m to the computated inverse of the matrix 
	m<-solve(data)
	## sets the value of the inverse in the cache
	x$setinv(m)
      ## returns a matrix that is the inverse of 'x'	
	m
}
