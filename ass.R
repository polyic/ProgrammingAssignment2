makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        setMatrix <- function(nv) {
                x <<- nv
                m <<- NULL
        }
        getMatrix <- function() { x } #store x into getMatrix
        cacInv <- function(inv) { m <<- inv}
        getInv <- function() { m }
        list(setMatrix = setMatrix, getMatrix = getMatrix,
		cacInv = cacInv, getInv = getInv)
}

mSolve <- function(y, ...) {
        inverse <- y$getInv()
        if(!is.null(inverse)) { #judge if the getInv() retrieved from y is NULL
                message("getting cached data")
                return(inverse)
        }
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacInv(inverse)
        inverse
}
