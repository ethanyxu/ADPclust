##' Automatically find centers with diagonal f(x) vs delta(x) thresholds
##'
##' @title Automatically find centers with diagonal f(x) vs delta(x) thresholds
##' @param f vector of local distance f(x)
##' @param delta vector of minimal distances to higher ground delta(x)
##' @param nclust number of clusters. Can be a vector.
##' @param rm.dup boolean. If TRUE (default) remove duplicated center vectors.
##' @return a list of vectors. Each vector gives the locations of centers.
##' @author Ethan Xu
FindCentersAutoD <- function(f, delta, nclust, rm.dup = TRUE){
    if(!is.numeric(nclust)) stop('arg nclust should inherit numeric. Got ', class(nclust))
    if(!all.equal(nclust, as.integer(nclust))) stop('nclust must all be integers')
    if(min(nclust) <= 0) stop('nclust must be positive integers')
    
    center.list <- list()

    x1 <- min(f); y1 <- max(delta)
    x2 <- max(f); y2 <- min(delta)
    pl.dist <- ((x2 - x1) * delta - (y2 - y1) * f + y2 * x1 - x2 * y1) / sqrt((y2 - y1) ^ 2 + (x2 - x1) ^ 2)
    cts <- order(pl.dist, decreasing = TRUE)[1:max(nclust)]
    for(i in seq_along(nclust)){
        centers <- cts[1:nclust[i]]
        attributes(centers) <- list(nclust = nclust[i])
        if(rm.dup){
            if(!IsDup(center.list, centers)){
                center.list <- c(center.list, list(centers))
            }
        }else{
            center.list <- c(center.list, list(centers))
        }
    }
    return(center.list = center.list)
}
    
