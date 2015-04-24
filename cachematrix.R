## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is used to create a special matrix object that can cache its own inverse

makeCacheMatrix<-function(x = numeric()){  #define the function makeCacheMatrix
        m <-NULL  #initialize variable m; m will be a free variable
        set <- function(y){
                x<<-y  #this function uses superoperator to assign local value (y) to x
                m<<-NULL  #m is reinitialized
        }
        get<- function() x  #get function simply returns the original input value (x)
        setinverse <-function(solve) m<<-solve #setinverse performs solve() function on m
        getinverse <-function() m #getinverse basically swaps x and m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes and returns the inverse of the special matrix object returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed) then the cacheSolve should return the inverse from the cache
## (i.e. return a matrix that is inverse of 'x')

cacheSolve <- function(x, ...){ #pass a matrix (x) to the function
        m<-x$getinverse()       #free variable (m) assigned to the getinverse() value of the matrix
        if(!is.null(m)){
                message("getting cached data") #if m has value then it is returned
                return(m)
        }
        data<-x$get()           #local variable data is assigned the original matrix value
        m<-solve(data, ...)     #free varialble, m, is assigned inverse value through solve
        x$setinverse(m)
        m
}