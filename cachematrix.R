## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates additional functions for creation of matrix
## set the value of matrix
## get the value of matrix
## set the value of inverse of matrix
## and gets the value of inverse of matrix
## values of x and mat are assigned from the parent env i.e. calling env
##  it has lexical scoping which  assignes val from the parent env i.e. calling env
## creates message if the matrix is not the object supplied or if it is not a square matrix.



library(MASS)
makeCacheMatrix <- function(x = matrix()) 
        {
        #check if it is a matrix object and it is a square matrix i.e. nrow equals ncol
        
        if (is.matrix(x) && ((nrow(x)) == (ncol(x))))
        {
                mat <- NULL
                set <- function(y)
                {
                        x <<- y
                        print(x)
                        mat <- NULL
                }
        
        get <- function()x
        setinv <- function(inv) mat <<- inv
        getinv <- function() mat
        list(set = set, get = get, setinv = setinv,getinv = getinv)
        }
        
        else{
                
                message("looks like it is not a matrix or square matrix")
        }

}







## Write a short comment describing this function
##  using "MASS" package to use ginv() function to calculate inverse of matrix
## if the matrix has size of < 10000 rows and columns then use Solve to compute the inverse but if the size is 
## gt than 10000 use ginv  time to run is 3.916769 hrs

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## return inverse of the original matrix in the global env i.e. from parent use the global env object

        mat <- x$getinv()
        if( !is.null(mat))
        {
                message("getting cached data for inverse")
                print("cached data procured")
                return(mat)
        }
        data <- x$get()
        
        if(ncol(data)< 10000)
                
        {       message("solve data")
                mat <- solve(data)
        }
        
        else
                {
                message("ginv data")
                mat <- ginv(data)
                 }
       
        x$setinv(mat)
        return(mat)

}




