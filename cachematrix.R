################################################################################
#
# The following two functions demonstrate R lexical scoping 
# file: cachematrix.R
# 
# makeCacheMatrix():  
#      input = Takes a square matrix() as its input arg
#     output = a list of functions to be used (fn1, fn2, fn3, ...)
#              by external environments - exposes fn's to be called by another
#              function in a different environment.
#
# cacheSolve():
#      input = The function output from makeCacheMatrix that contains the
#              object value "x" as an externally available var whose environment
#              is scoped to be accessed outside of the makeCacheMatrix() function.
#     output = The inverse of the original matrix provide as input to 
#              makeCacheMatrix().
#
# Resources:
# Original stub function provided by Programming R course instructor:
# https://class.coursera.org/rprog-011/human_grading/view/courses/973492/
# assessments/3/submissions
#
# Additional credit: 
#  forum - https://class.coursera.org/rprog-011/forum/thread?thread_id=771
#        - https://class.coursera.org/rprog-011/forum/thread?thread_id=405
#        - https://class.coursera.org/rprog-011/forum/thread?thread_id=538
#        - https://class.coursera.org/rprog-011/forum/thread?thread_id=625
# git/github
# http://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup
# 
#
#
################################################################################

makeCacheMatrix <- function(x = numeric()) {
        ##############################################################
        # initialize external/"exported" variable
        ##############################################################
         m <- NULL                                 
        
        ##############################################################
        # assign input matrix to "x"
        ##############################################################
        get <- function() x                       
        
        ##############################################################
        # initialize the value to be applied to setmatrix
        ##############################################################
        setmatrix <- function(solve) m <<- solve  
        
        ##############################################################
        # return matrix set above
        ##############################################################
        getmatrix <- function() m                
        
        ##############################################################
        # return list of functions for external env use
        ##############################################################
        list(get = get,                           
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) {
        
        
        ##############################################################
        # retreive the cached value from makeCacheMatrix as exposed
        ##############################################################
        m <- x$getmatrix()                       
        
        ##############################################################
        # check for existence of matrix in memory        
        # if the value of m is not null, the matrix must already
        # exist, so simply return it.
        ##############################################################
        if(!is.null(m)) {                        
                message("retrieving cached matrix data")
                return(m)
        }
        
        ##############################################################
        # If the matrix value was NULL then retreive inputed matrix
        # calculate the inverse
        # cache the matrix inverse abck to the scope from makeCacheMatrix 
        # return the newly calculated matrix inverse
        ##############################################################
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m 
}
