##makeCacheMatrix accepts a matrix m as an argument.  It has methods set and get for setting and getting the matrix
##It maintains the value of the inverse of the matrix, initialized to NULL upon construction or after set is called.
##the setInverse and getInverse methods set and retrieve the inverse of m. the get/set IsInvertible methods record
#the invertibility of the matrix if known.

makeCacheMatrix<-function(m=matrix()){
    #first make sure it is non-Null, square and numeric
    if(is.null(m)){ message("Null matrix")}
    if(nrow(m)!=ncol(m)){ message("This matrix is not invertible because it is not square")}
    if(!is.numeric(m)){ message("This is not a numeric matrix")}
    
    #define variables to stor invertibility and the inverse
    isInvertible<-NULL
    inv<-NULL
    #method to set the value of the matrix
    set<- function(newMatrix){
        m<<-newMatrix
        inv<-NULL
        isInvertible<-NULL
    }
    get <-function() m #method to get the matrix value
    
    
    setInverse<-function(inverse){ 
        if(is.null(inverse)){isInvertible<<-F
        }else{inv<<-inverse;isInvertible<<-T }}   #method to set the value of the inverse
    getInverse<-function() inv #method to set the value of the inverse
    setIsInvertible<-function(bool) isInvertible<<-bool #method to set the invertibility of the matrix
    getIsInvertible<-function() isInvertible #method to get the invertibility of the matrix
    L<-list(set=set,get=get,setInverse=setInverse,getInverse=getInverse,setIsInvertible=setIsInvertible,
            getIsInvertible=getIsInvertible)
    invisible(L) #suppresses useless printing
    
}

#This function will retrieve a cached inverse if known, and otherwise find the inverse if possible.

cacheSolve<-function(m,...){
    #retrieving cached inverse and invertibility and matrix
    inv<-m$getInverse()
    isInvertible<-m$getIsInvertible()
    mat<-m$get()
    #find the inverse and set it
    findInverse<-function(){inv<-solve(mat); m$setInverse(inv)}
    
    #Try to set the inverse if it is null, stop if the matrix is not invertible, retrieve the Cached value if it exists
    if(is.null(inv) &is.null(isInvertible)){
        tryCatch(findInverse(), error= function(e){print("Error inverting the matrix, it may not be invertible") 
            ;m$setIsInvertible(F)}  ) }
    else if(isInvertible==F){stop("The matrix has previosuly been found to be not invertible") }
    else{message("Retrieving cached matrix inverse");return(inv)} 
    
}