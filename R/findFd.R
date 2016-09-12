##' Find f(x) and delta(x) from distm, given h. 
##'
##' Find f(x) and delta(x).
##' @title Find f and delta from distance matrix.
##' @param distm distance matrix of class 'dist'
##' @param h bandwidth
##' @param fdelta String. One of [unorm, mnorm, weighted, count]
##' @export
##' @return list of f and delta. Each is a vector.
##' @author Ethan Xu

FindFD <- function(distm, h, fdelta){
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

    ## Find delta(x)
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
