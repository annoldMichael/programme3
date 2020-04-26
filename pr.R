##########################################

##Cette fonction crée un objet "matrice" spécial qui peut mettre en cache son inverse.

#########################################

makeCacheMatrix <- function(x = matrix()) {
  
  xMat<-makeCacheMatrix(x)  
  parent.env(xMat$getenv())$m  
  environment(xMat$getmean)  
  m<-NULL   
  evn <- environment()  
  y<-NULL 
  
  setmatrix<-function(y){  
    x<<-y  
    m<<-NULL 
  }
  
  getmatrix<-function() x 
  setinverse<-function(solve) m<<- solve  
  getinverse<-function() m 
  getenv<- function() environment()
  
  list (setmatrix=setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
  
}

###################################
## Cette fonction calcule l'inverse de la "matrice" spéciale retournée par makeCacheMatrix
ci-dessus. Si l'inverse a déjà été calculé (et que la matrice n'a pas changé),
le cachesolve doit récupérer l'inverse dans le cache.
###################################

cacheSolve <- function(xMat= m(), ...) {
 
  m <- xMat$getinverse() 
  if(!is.null(m)){ 
    if(xMat$setmatrix() == xMat$getmatrix()) { 
      message("getting cached data")
      matrix<-xMat$get()
      m<-solve(matrix, ...)
      xMat$setmatrix(m)
      return(m) 
    }
    
    y <- xMat$getmatrix() 
    xMat$setmatrix(y) 
    m <- solve(y, ...) 
    xMat$setinverse(m) 
    m 
  }
} 