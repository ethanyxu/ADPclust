FindCentersAutoV <- function(f, delta, f.cut = c(0.1, 0.2, 0.3), nclust, rm.dup = TRUE){
    center.list <- list()
     
    for(i in seq_along(f.cut)){ # For each f.cuts
        ##f0 <- min(f) + f.cut[j] * (max(f) - min(f))
        f0 <- quantile(f, probs = f.cut[k])
        delta1 <- delta
        delta1[f < f0] <- -Inf
        cts <- order(delta1, decreasing = TRUE)[1:max(nclust)]
        for(j in seq_along(nclust)){
            centers <- cts[1:nclust[j]]
            attributes(centers) <- list(f.cut = f.cut[i], nclust = nclust[j])
            if(rm.dup){
                if(CheckDup(center.list, centers)){
                    center.list <- c(center.list, centers)
                }
            }else{
                center.list <- c(center.list, centers)
            }
            
        }
    }
    return(center.list = center.list)
}


FindCentersAutoD <- function(f, delta, nclust, rm.dup = TRUE){
    center.list <- list()

    x1 <- min(f); y1 <- max(delta)
    x2 <- max(f); y2 <- min(delta)
    pl.dist <- ((x2 - x1) * delta - (y2 - y1) * f + y2 * x1 - x2 * y1) / sqrt((y2 - y1) ^ 2 + (x2 - x1) ^ 2)
    cts <- order(pl.dist, decreasing = TRUE)[1:max(nclust)]
    for(i in seq_along(nclust)){
        centers <- cts[1:nclust[i]]
        attributes(centers) <- list(f.cut = f.cut[i], nclust = nclust[j])
        if(rm.dup){
            if(CheckDup(center.list, centers)){
                center.list <- c(center.list, centers)
            }
        }else{
            center.list <- c(center.list, centers)
        }
    }
    return(center.list = center.list)
}
    
