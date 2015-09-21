######### This script is an example of cache method that simplyfies time 
######### consuming computations
##
## Here we are calculating the inverse of a matrix memorize the results
## in order to speed up the repetitive function calls


## The following function creates a list of 4 elements(functions) 
## that will be used by the main function (cacheSolve) to look 
## in the cache and check if same computation has been done before

makeCacheMatrix=function(x=matrix())
      {
      inv=NULL
      set=function(y) {
      x<<-y
      inv<<-NULL
                      }
      get=function()x
      setsolve=function(solve) inv<<-solve
      getsolve<-function()inv
      list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
      }

                                                                                   
                                                    
## Next function will calculate the inverse of a matrix
## First checks for inverse value created with makeCacheMatrix. Options:
##                      (a) not NULL if calculation was found in cache
##                      (b) NULL if calculation was not found
## if (a), the inverse is retrieved from cache and returned
## if (b), the calculations are done, pushed to cache and inverse returned

cacheSolve <- function(x, ...) 
           {
  ## Return a matrix that is the inverse of 'x' 
      inv=x$getsolve()
      if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
                        }
      data=x$get()
      inv=solve(data,...)
      x$setsolve(inv)
      inv
            }
