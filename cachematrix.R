## This function will first check if the matrix is invertible by checking the determinant.
## If determinant is zero, it will exit the function.
## Output of this makeCacheMatx is a list with the following function
## 1: setMtx to set the matrix
## 2: getMtx to get the matrix
## 3: setInverseMtx to set the inverse matrix
## 4: getInverseMtx to get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## exit the function if determinant zero
        if(det(x)==0){
          stop("Matrix is not invertible because the determinant for this matrix is zero (singular)")
        }
        inverseMtx <- NULL
        setMtx <- function(y) {
                x <<- y
                inverseMtx <<- NULL
        }
        getMtx <- function() x
        setInverseMtx <- function(inverse) inverseMtx <<- inverse
        getInverseMtx <- function() inverseMtx
        list(setMtx = setMtx, 
             getMtx = getMtx,
             setInverseMtx = setInverseMtx,
             getInverseMtx = getInverseMtx)
}


## Following function will calculate the inverse of the matrix.
## It will first check to see if the inverse has already been calculated. If so,
##   it gets the inverse from the cache and skip the calculation.
## If the matrix has not been calculated, it calculates the inverse
##   and store it to cache via the "setInverseMtx"

cacheSolve <- function(x, ...) {
        inverseMtx <- x$getInverseMtx()
        ## get the inverse if it has already been calculated
        if(!is.null(inverseMtx)) {
              message("getting Inverse result from cached data")
              return(inverseMtx)
        }
        ## otherwise calculate the inverse matrix and store it to cache
        message("not getting cached data")
        dataMtx <- x$getMtx()
        inverseMtx <- solve(dataMtx, ...)
        x$setInverseMtx(inverseMtx)
        inverseMtx
}
## example how to run the function:
##   > myvector <- makeVector(x)
##   > cachemean(myvector)

