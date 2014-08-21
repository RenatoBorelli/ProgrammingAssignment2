
#################################################################                      
##                      Jose Renato Borelli                     #
##                      Campinas,SP - Brasil.                   #
##                                                              #
##            Thanks a lot Coursera and Professors!             #
#################################################################



## makeCacheMatrix receives as argument a matrix and creates for it a struct (List), 
## containing two atributes: 
## - the matrix by itself
## - its inverse, initially Null.
## The function also creates an interface with the functions: set, get, setInv and getInv.
## This structure is stored in memory.
## Utilization: create a matrix m<-matrix(1:4,2,2), 
## then create the structure mCh<-makeCacheMatrix(m)


makeCacheMatrix <- function(mtrx = matrix()) {
        
                inv <- NULL
                
                set <- function(y) {
                        mtrx <<- y
                        inv <<- NULL
                }
                
                get <- function() mtrx
                setInv <- function(mtrxinv) inv <<- mtrxinv
                getInv <- function() inv
                list(set = set, get = get,
                     setInv = setInv,
                     getInv = getInv)
        
}


## cacheSolve receives as argument a structure created by makeCacheMatrix.
## It verifies if the structure already have a calculated Inverse, using getInv() of the structure interface.
## There is a detail at this point. If the passed argument is a matrix, not a structure created by makeCacheMatrix,
## the function will present an error (Error in cacheSolve(c) : object 'inv' not found). 
## Its necessary to orient the user: "Please, you need to use makeCacheMatrix first: mtrxMem<-makeCacheMatrix(mtrx)".
## This is the reason I used the tryCatch function at that test.
##
## In case the argument is correct, the function test if there is already an inverse calculated at the cache.
## If not, it calculates the inverse, using solve(), and uses the setInv() to set this value at the structure.
## Another detail is that, a basec test for inversible matrix is that it must be square. 
## This is the reason I inserted the test: if(dim(data)[1]!= dim(data)[2]) return("The matrix must be square") 
##

cacheSolve <- function(mtrx, ...) {
        
        
                tryCatch ({inv <- mtrx$getInv()},
                          error= function(e) {print("Please, you need to use makeCacheMatrix first: mtrxMem<-makeCacheMatrix(mtrx)")})
                           
                           
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                
                
                data <- mtrx$get()
                
#  Basic test to be Inversible:
                if(dim(data)[1]!= dim(data)[2]) return("The matrix must be square")

                inv <- solve(data, ...)
                mtrx$setInv(inv)
                
                inv
        
}
