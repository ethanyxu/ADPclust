##' Calculate f(x) and delta(x) from distm and h. 
##'
##' @title Find f and delta from distance matrix.
##' @param distm distance matrix of class 'dist'.
##' @param h bandwidth.
##' @param fdelta character string that specifies the method used to estimate local density f(x) at each data point x. The default is "mnorm" that uses a multivariate Gaussian density estimation to calculate f. Other options are listed below. Here 'distm' denotes the distance matrix. 
##' \itemize{
##' \item{unorm}{(f <- 1/(h * sqrt(2 * pi)) * rowSums(exp(-(distm/h)^2/2))); Univariate Gaussian smoother}
##' \item{weighted}{(f <- rowSums(exp(-(distm/h)^2))); Univariate weighted smoother}
##' \item{count}{(f <- rowSums(distm < h) - 1); Histogram estimator (used in Rodriguez [2014])}
##' }
##' @export
##' @return list of two items: f and delta. 

FindFD <- function(distm, h, fdelta){
    # -------------------------------------------------------------------------    
    # Check arguments
    # -------------------------------------------------------------------------       
    if(!inherits(distm, 'dist')) stop("arg distm must inherit dist class. Got ", class(distm))
    
    n <- attr(distm, 'Size')
    ## Find f(x)
    distm <- as.matrix(distm)
    if(fdelta == "unorm"){
        f <- 1/(h * sqrt(2 * pi)) * rowSums(exp(-(distm/h)^2/2))
    }else if(fdelta == "weighted"){
        f <- rowSums(exp(-(distm/h)^2))        
    }else if(fdelta == "count"){
        f <- rowSums(distm < h) - 1   
    }else if(fdelta == "mnorm"){
        f <- rowSums(exp(-(distm / h) ^ 2 / 2))
    }else{
        stop("Wrong fdelta, try 'unorm', 'weighted', 'count' or 'mnorm' (recommended).")
    }

    # -------------------------------------------------------------------------    
    # Find f and delta
    # -------------------------------------------------------------------------       
    if(fdelta == "count"){
        f1 <- rank(f, ties.method = "first") # Break ties in f
        delta <- apply(distm / outer(f1, f1, FUN = ">"), 2, min, na.rm = TRUE)
        loc.max <- which.max(delta)
        delta[loc.max] <- max(delta[-loc.max]) # Equation in the Matlab code
    }else if(fdelta == "mnorm"){
        f.order <- order(f, decreasing = TRUE)
        delta <- rep(NA, n)
        delta[f.order[1]] <- Inf
        for(i in 2:length(f.order)){
            delta[f.order[i]] <- min(distm[f.order[i], f.order[1:(i - 1)]])
        }
        delta[f.order[1]] <- max(delta[-f.order[1]])
    }else{
        delta <- apply(distm / outer(f, f, FUN = ">"), 2, min, na.rm = TRUE)
        loc.max <- which.max(delta)
        delta[loc.max] <- max(delta[-loc.max]) # Equation in the Matlab code
    }

    return(list(f = f, delta = delta))
}
