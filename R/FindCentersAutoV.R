##' Automatically find centers with vertical threshold vertical f(x) thresholds.
##'
##' Given f's and delta's, cluster centers are chosen to be the data points whose delta values are high and f values are larger than a fixed threshold. To be more specific, let F denote the set of all f(x). centers are selected as points with the largest m delta values in the set {x | f(x) > a'th percentile of F}. The number of centers m is given by the parameter nclust. The cutting percentile a is given by the parameter f.cut. When at least one of these two parameters are vectors, centers are selected based all combinations of them, and returned in a list.
##'
##' @title Automatically find centers with vertical threshold
##' @param f vector of local distance f(x). See the detail section of the help(adpclust).
##' @param delta vector of minimal distances to higher ground delta(x). See the detail section of the help(adpclust).
##' @param nclust number of clusters. It can be either a single integer or a vector of integers.
##' @param f.cut number between (0, 1) or numeric vector of numbers between (0, 1). Data points whose f values are larger than f.cut with large delta values are selected as centers. The default is c(0.1, 0.2, 0.3).
##' @param rm.dup boolean. If TRUE (default) duplicated centers vectors are removed from returned list.
##' @export
##' @return a list of vectors. Each vector contains the indices of selected centers.
##' @author Ethan Xu
FindCentersAutoV <- function(f, delta, f.cut = c(0.1, 0.2, 0.3), nclust, rm.dup = TRUE){
    # -------------------------------------------------------------------------
    # Check arguments
    # -------------------------------------------------------------------------    
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
        cts <- order(delta1, decreasing = TRUE)[1 : max(nclust)]
        for(j in seq_along(nclust)){ # For each nclust
            if(sum(f >= f0) < nclust[j]){ # Number of points that > f.cut is less than nclust. Stop
                stop("Only (", sum(f >= f0), ") points to the right of f.cut (", f0, "), but nclust = ", nclust[j])
            }
            centers <- cts[1 : nclust[j]]
            attributes(centers) <- list(f.cut = f.cut[i], 
                                        f.cut.value = f0,
                                        nclust = nclust[j])
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
