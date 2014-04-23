## Custom matrix data type that allows caching of the matrix inverse.

## Creates a 'cache matrix' object representing the matrix x.
## 
## A 'cache matrix' is a list with four methods: getValue, setValue,
## getInverse, and setInverse. The matrix value, x,  is retrievable
## by invoking the setValue method, and can be updated by invoking
## setValue with the new value as its only parameter. The inverse of 
## the matrix can also be cached by passing the value to setInverse
## (no validation is performed) and retrieved by calling getInvsere.
## Calls to setValue invalidate the cached inverse, setting it to NULL.
makeCacheMatrix <- function(x = matrix()) {
    # Closure variable for caching the inverse
    inverse <- NULL
    
    setValue <- function(newX) {
        x <<- newX
        message('matrix changed; invalidating cached inverse (if any)')
        inverse <<- NULL
    }
    
    getValue <- function() {
        x
    }
    
    setInverse <- function(newInverse) {
        message('setting cached inverse')
        inverse <<- newInverse
    }
    
    getInverse <- function() {
        inverse
    }
    
    # This is exactly analagous to the so-called "revealing module pattern" used in Javascript:
    # 
    # function makeObject() {
    #   function myMethod() { 
    #     return doStuff(); }
    #   }
    # 
    #   return {
    #     myMethod: myMethod
    #   };
    # }
    # // later:
    # obj = makeObject(); obj.myMethod();  // does stuff
    
    list(setValue=setValue, getValue=getValue, getInverse=getInverse, setInverse=setInverse)
}


## Given a 'cache matrix' object, x, returns the inverse of the matrix 
## represented by x. If a cached value of the inverse exists, it is used; if there
## is no cache value, the inverse (as computed by solve()) is cached into x.
## Throws if the matrix is singular.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) {
        message('using cached inverse')
        return(inverse)
    }
    
    xValue <- x$getValue()
    inverse <- solve(xValue)
    x$setInverse(inverse)
    inverse
}
