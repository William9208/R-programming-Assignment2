## cachematrix.R
## allow the inverse of a matrix to be cached

## makeCacheMatrix: cache the results

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function()x
	setinverse<-function(inverse)i<<-inverse
	getinverse<-function()i
	list(set=set,
	get=get,
	setinverse=setinverse,
	getinverse=getinverse)
}


## cacheSolve: retuen a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	i<-x$getinverse()
	if(!is.null(i)){
		messages("Retrieving cached data.")
		retuen(i)
	}
	data<-x$get()
	i<-solve(data,...)
	x$setinverse(i)
	i
}
## SHA-1:a9dc6530daf2cccbe3170550ac06ac4408518cfe
