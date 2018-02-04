## Put comments here that give an overall description of what your
## functions do

##Matrix inverse is costly computation, instead of compute inverse repeatedly,
##this function has a pair functions compute matrix inverse and cache the inverse
##makeCacheMatrix:creates an object that caches its matrix inverse.
##cacheSolve:computes the inverse of matrix returned by makeCacheMatrix

## Write a short comment describing this function
##This function basically create four sub functions inside
##to cache matrix inverse. The set function is used to return the input matrix
##to parent environment, the get function is used to read the cached
## matrix, the setinv function is used to return the input matrix inverse to  
##parent environment, the getinv function is used to read the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
            x<<-y
            inv<<-NULL
      }
      get<-function() x
      setinv<-function(inverse) inv<<-inverse
      getinv<-function() inv
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
##This function first test if the matrix inverse already has been computed
##if it is, return the matrix inverse, if not, compute the inverse and cache 
##in the makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data<-x$get()
      inv<-solve(data)
      x$setinv(inv)
      inv
}
