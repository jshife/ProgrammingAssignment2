#Because the inverse of a matrix can take awhile to compute, this program contains
#two functions which allow us to create a matrix object whose inverse can be cached
#Therefore, the inverse only has to be calculated once


#the makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#the object returned contains a list of four functions: set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
        
        #initialize the inverse variable in the makeCacheMatrix environment
        inv <- NULL

        #function to set the value of the matrix
        set <- function(y) {
                #use <<- operator to set the values in the makeCacheMatrix environment
                #make sure to reset the inverse when setting new value of matrix
                x <<- y
                inv <<- NULL
        }


        #function to return matrix
        get <- function() x


        #function to set inverse (used in cacheSolve)
        setinverse <- function(inverse) inv <<- inverse


        #function to return the inverse of the matrix
        getinverse <- function() inv


        #return the list with the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




#the cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
#retrieves the inverse from the cache
cacheSolve <- function(x, ...) {

        #retrieve inverse from object
        inv <- x$getinverse()

        #if inverse has already been calculated return the cached inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        #calculate the inverse if it has not already been cached and return it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}





#test code
test <- makeCacheMatrix()
a <- matrix(c(1,5,6,4,2,1,3,8,9),nrow=3,ncol=3)

test$set(a)           #set the matrix of the object
test$get()            #should return the same matrix a
test$getinverse()     #should be null
cacheSolve(test)      #calculate the inverse
test$getinverse()     #should now contain the inverse
cacheSolve(test)      #should use cache instead of recalculating

