##' Calculate the AMISE bandwidth from either a data frame, or from the number of observations and the dimension of the data.
##'
##' IMPORTANT NOTE: The standard deviation of each variable is omitted in this formula.
##'
##' @title AMISE bandwidth
##' 
##' @export
##' 
##' @param x the number of variables (if y is given), or a data frame or a matrix (if y is missing).
##' @param y the number of observations. If y is missing then x is interpreted as the data matrix.
##' @return AMISE bandwidth.
AMISE <- function(x, y = NULL){
    if(is.null(y)){
        if(inherits(x, c("data.frame", "matrix"))){
            n <- nrow(x)
            p <- ncol(x)
        }
    }else{
        if(is.numeric(x) && x > 0 && is.numeric(y) && y > 0){
            p = x
            n = y
        }else
            stop("Wrong x, y.")
    }
    h <- (4 / (p + 2)) ^ (1 / (p + 4)) * n ^ (-1 / (p + 4))
    return(h)
}
