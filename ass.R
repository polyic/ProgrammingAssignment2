makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        setMatrix <- function(nv) {
                x <<- nv
                m <<- NULL
        }
        getMatrix <- function() { x }
        cacInv <- function(inv) { m <<- inv}
        getInv <- function() { m }
        list(setMatrix = setMatrix, getMatrix = getMatrix,
		cacInv = cacInv, getInv = getInv)
}

mSolve <- function(y, ...) {
        inverse <- y$getInv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacInv(inverse)
        inverse
}