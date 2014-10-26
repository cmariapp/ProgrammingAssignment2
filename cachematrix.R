# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
# These  functions assumes that the matrix is always invertible.


# Function makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
# It will return a list with the following four functions
# set - set the value of the matrix
# get - get the value of the matrix
# setinverse - set the value of inverse of the matrix
# getInverse - get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) {
    	#caches the values
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Function cacheSolve returns the inverse of the matrix. 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then it retrieve the inverse from the cache and return the value.


cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        print("Getting the matrix inverse from cache.")
        return(inv)
    }
    #calculate inverse of matrix using solve function
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


#Test function
test = function(mat)
{
        ## @mat: an invertible matrix
        
        temp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print ("inverse calulated")
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print ("inverse from cache")
        print(dur)
}



#> set.seed(9999999)
#> r = rnorm(1000000)
#> mat1 = matrix(r, nrow=1000, ncol=1000)
#> test(mat1)
#[1] "inverse calulated"
#Time difference of 1.087062 secs
#[1] "Getting the matrix inverse from cache."
#[1] "inverse from cache"
#Time difference of 0.001000166 secs
