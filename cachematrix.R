## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is used to create a special matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL  #initialize the cached mean, m, which is a free variable defined in parent env.
	set <- function(y) { #set the value for the matrix (x)
		x <<- y    #use <<- to assign the user-defined matrix (y) to the locally defined matrix(x)
            print(dim(x)) #use this print to see the dimensions of the matrix 
		m <<- NULL  #re-initialize the cached mean, now within the local environment
	}
	get <- function() x	#get the value of the matrix x
	setmean <- function(mean) m <<- mean	#set the value of the matrix mean with the value of the cached mean
	getmean <- function() m	#get the cached mean
	list(set = set, get = get, #creates a list of functions which can be passed into another function to use
             setmean = setmean,
             getmean = getmean)
	ls(environment(set)	#for reference, show what's in the function "set" environment
	ls(environment(get)	#for reference, show what's in the function "get" environment
}


## cacheSolve computes and returns the inverse of the special matrix object returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed) then the cacheSolve should return the inverse from the cache
## (i.e. return a matrix that is inverse of 'x')

cacheSolve <- function(x, list(set, get, setmean, getmean) {#pass x (special matrix) to the function.  Likewise, due to lexical scoping, the functions get, set, 
						#getmean and setmean need to be passed to the cacheSolve function
        m <- x$getmean()		#use getmean function of matrix x to assign cached mean (m)
        if(!is.null(m)) {
                message("getting cached data") #if m is a non-empty matrix then alert the user and return m
                return(m)
        }

	  data <- x$get()	#assigns a local variable 'data' the value of the matrix (x)
        m <- mean(data, ...) #now the cached mean (m) is assigned the mean of the 'data' matrix
        x$setmean(m)	#now the setmean function resets the mean of x as the cached mean, effectively creating an inverse 
        m			#return the inverse
}
}
