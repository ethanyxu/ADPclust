adpclust <- function(x, # data matrix
                     distm = NULL, # distance matrix. ignored if x is given
                     p = NULL, # ignored if x is given
                     centroids, # [user, auto]
                     h = NULL, # bandwidth
                     htype, # [amise, rot] methods to calculate h (ignored if h is not null)
                     nclust, # number of clusters
                     ac, # [1,2] automatic cutting method
                     f.cut, # cutting percentile for cutting method 1. ignored if ac == 2
                     fdelta, # [mnorm, unorm, weighted, count] methods to calculate fdelta
                     dmethod, # methods to calculate distance matrix. ignored if distm != null.
                     verbose,
                     draw,
                     findSil
                     ){
    if(is.null(x)){
        # Use distm
        if(is.null(distm)){
            stop("Must provide one of x or distm")
        }
        if(is.null(p) && is.null(h)){
            stop("Bandwidth h and data x are not given. Must provide p to calculate h.")
        }
    }else{
        # Use x
        if(fdelta == "mnorm"){
            distm <- FindDistm(x, standardize = TRUE, method = "euclidean")
        }else{
            distm <- FindDistm(x, standardize = FALSE, method = dmethod)
        }
        p = ncol(x)
    }
    
    if(is.null(h)){
        if(fdelta != "mnorm"){
            stop("Must give h unless fdelta == 'mnorm'")
        }
        h <- FindH(p, ncol(distm), htype)
    }

    if(centroids == "user"){
        if(length(h) > 1){
            stop("h must be a scalar when centroids == 'user'")
        }
        fd <- FindFD(distm, h, fdelta)
        return(FindClusterManual(distm, fd$f, fd$delta))
    }

    if(centroids == "auto"){
        if(length(h) > 1){
            h.seq <- h
        }else{
            h.seq <- seq(h / 3, h * 3, length.out = 10)
        }
        fd.seq <- apply(h.seq, function(h) FindFd(distm, h, fdelta))
        return(FindClusterAuto(distm, fd$f, fd$delta, nclust, ac, f.cut))
    }
}
