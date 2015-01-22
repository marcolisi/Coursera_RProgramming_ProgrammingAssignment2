## The makeCacheMatrix instantiates a list that holds a specified matrix and 
## functions to set and get the matrix, and holds an inverse of the matrix as 
## well as to set and get the inverse

makeCacheMatrix <- 
    function(
        theMatrix = matrix()
    ) 
    {
        assertThatTheMatrixIsInvertible(
            theMatrix
        )
        
        theInverse <- 
            NULL
        
        #---
        
        set <-
            function(
                theNewMatrix
            )
            {
                theMatrix <<-
                    theNewMatrix
                
                # The cached inverse is invalidated everytime the matrix is set
                theInverse <<-
                    NULL
            }
        
        #---
        
        get <-
            function()
            {
                theMatrix
            }
        
        #---
        
        setInverse <-
            function(
                theInverseOfTheMatrix
            )
            {
                theInverse <<-
                    theInverseOfTheMatrix
            }
        
        #---
        
        getInverse <-
            function()
            {
                theInverse
                
                # another option would be to calculate and set the inverse
                # here, instead of needing the call to an external function
                # if (is.null(theInverse))
                # {
                #     theInverse <<-
                #         solve(
                #             theMatrix
                #         )
                # }
                # else
                # {
                #     message("Getting the cached inverse.")
                # }
                #
                # theInverse
            }
        
        #---
        
        theCacheMatrix <-
            list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
            )
        
        theCacheMatrix
    }


## The cacheSolve takes in a cacheMatrix "object" and sets its inverse, if the
## inverse as not already been calculated
## --
## Another option would be: instead of having an external function set the
## inverse, that the inverse is calculated and cached when the getInverse
## function is called on the cacheMatrix. Would be much more cohesive design
## from an OO perspective, considering that the cacheMatrix is a sort of object
## with getter and setter functions within it.

cacheSolve <-
    function(
        theCacheMatrix
    ) 
    {
        theInverse <-
            theCacheMatrix$getInverse()
        
        if (is.null(theInverse))
        {
            theMatrix <- 
                theCacheMatrix$get()
            
            theInverse <-
                solve(
                    theMatrix
                )
            
            theCacheMatrix$setInverse(
                theInverse
            )         
        }
        else
        {
            message("Getting the cached inverse.")
        }
        
        theInverse
    }

#---

assertThatTheMatrixIsInvertible <-
    function(
        theMatrix
    )
    {
        theNumberOfRowsOfTheMatrix <-
            nrow(
                theMatrix
            )
        
        theNumberOfColumnsOfTheMatrix <-
            ncol(
                theMatrix
            )
        
        theMatrixIsInvertible <-
            theNumberOfRowsOfTheMatrix == theNumberOfColumnsOfTheMatrix
        
        assert(
            "The specified matrix is not invertible.",
            theMatrixIsInvertible
        )
    }

assert <-
    function(
        message,
        ...
    )
    {
        if (!all(...))
        {
            print(message)
            stop(...)
        }
    }