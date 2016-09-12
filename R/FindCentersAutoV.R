##' Automatically find centers with vertical f(x) thresholds
##'
##' @title Automatically find centers with vertical f(x) thresholds
##' @param f vector of local distance f(x)
##' @param delta vector of minimal distances to higher ground delta(x)
##' @param nclust number of clusters. Can be a vector.
##' @param f.cut number between (0, 1) or numeric vector of numbers between (0,1). f.cut is used in centroids = "auto" to automatically select cluster centroids from the decision plot. Points with f(x) > f.cut and high delta(x) are selected as one set of candidate centroids (see details). Default = c(0.1, 0.2, 0.3).
##' @param rm.dup boolean. If TRUE (default) remove duplicated center vectors.
##' @return a list of vectors. Each vector gives the locations of centers.
##' @author Ethan Xu
FindCentersAutoV <- function(f, delta, f.cut = c(0.1, 0.2, 0.3), nclust, rm.dup = TRUE){
    if(!is.numeric(f.cut)) stop('arg f.cut should inherit numeric. Got ', class(f.cut))
    if(length(f.cut) == 0) stop('arg f.cut is empty: ', f.cut)    
    if(min(f.cut) < 0) stop('arg f.cut must be between 0 - 1. Got', f.cut)
    if(max(f.cut) >= 1) stop('arg f.cut must be between 0 - 1. Got', f.cut)    
    if(!is.numeric(nclust)) stop('arg nclust should inherit numeric. Got ', class(nclust))
    if(!all.equal(nclust, as.integer(nclust))) stop('nclust must all be integers')
    if(min(nclust) <= 0) stop('nclust must be positive integers')    

    center.list <- list()
     
    for(i in seq_along(f.cut)){ # For each f.cuts
        ##f0 <- min(f) + f.cut[j] * (max(f) - min(f))
        f0 <- stats::quantile(f, probs = f.cut[i])
        delta1 <- delta
        delta1[f < f0] <- -Inf
        cts <- order(delta1, decreasing = TRUE)[1:max(nclust)]
        for(j in seq_along(nclust)){
            if(sum(f >= f0) < nclust[j]){ # Number of points that > f.cut is less than nclust. Stop
                stop("Only (", sum(f >= f0), ") points to the right of f.cut (", f0, "), but nclust = ", nclust[j])
            }
            centers <- cts[1:nclust[j]]
            attributes(centers) <- list(f.cut = f.cut[i], f.cut.values = f0)
            if(rm.dup){
                if(!IsDup(center.list, centers)){
                    center.list <- c(center.list, list(centers))
                }
            }else{
                center.list <- c(center.list, list(centers))
            }
            
        }
    }
    return(center.list = center.list)
}
