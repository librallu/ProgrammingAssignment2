## author : librallu
## This file is divided into three parts :
##  - makeCacheMatrix(x) that returns a special object for caching 
##    inverse of the matrix x
##  - cacheSolve(x) that computes inverse if needed
##  - Some tests for the previous functions


##  makeCacheMatrix(x) that returns a special object for caching 
##    inverse of the matrix x
##   - x : matrix we want to cache
##   - returns : special object
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL # if w make changes in the object, we need to forget
					 # old inverse value
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv

	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(x,...) computes the inverse if needed and returns it
##  - x : special object for getting the inverse
##  - returns : inverse of x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if ( !is.null(inv) ){ # if we have cached value
			message("getting chached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data)
		x$setinv(inv)
		inv
}


# Some tests

print("displaying initial matrix")
m <- matrix(data=c(1,2,3,4), ncol=2)
print(m)

obj <- makeCacheMatrix(m)
print("displaying object value")
print(obj$get())

print("Displaying the inverse")
print(cacheSolve(obj))

print("Caching test")
print(cacheSolve(obj))

print("Make changes in the object")
obj$set(matrix(data=c(4,3,2,1), ncol=2))
print(obj$get())

print("displaying the inverse")
print(cacheSolve(obj))

print("caching test")
print(cacheSolve(obj))
