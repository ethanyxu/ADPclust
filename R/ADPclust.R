##' @title Fast Clustering Using Adaptive Density Peak Detection
##' 
##' @description Clustering of data by finding cluster centers from estimated density peaks. ADPclust is a non-iterative procedure that incorporates multivariate Gaussian density estimation. The number of clusters as well as bandwidths can either be selected by the user or selected automatically through an internal clustering criterion.
##' 
##' @details Given n data points x's in p dimensions, adpclust() calculates f(x) and delta(x) for each data point x, where f(x) is the local density at x, and delta(x) is the shortest distance between x and y for all y such that f(x) <= f(y). Data points with large f and large delta values are labeled class centroids. In other words, they appear as isolated points in the upper right corner of the f vs. delta plot (the decision plot). After cluster centroids are determined, other data points are clustered according to their distances to the closes centroids.
##' 
##' A bandwidth (smoothing parameter) h is used to calculate local density f(x) in various ways. See parameter 'fdelta' for details. If centroids = 'user', then h must be explicitly provided. If centroids = 'auto' and h is not specified, then it is automatically selected from a range of testing values: First a reference bandwidth h0 is calculated by one of the two methods: Scott's Rule-of-Thumb value (htype = "ROT") or Wand's Asymptotic-Mean-Integrated-Squared-Error value (htype = "AMISE"), then 10 values equally spread in the range [1/3h0, 3h0] are tested. The value that yields the highest silhouette score is chosen as the final h.  
##'
##' @param x numeric data frame where rows are observations and columns are variables. One of x and distm must be provided.
##' @param distm distance matrix of class 'dist'. distm is ignored if x is given.
##' @param p number of variables (ncol(x)). This is only needed if neither x nor h is given.
##' @param centroids character string specifying how cluster centroids are selected. Valid options are "user" and "auto". 
##' @param h nonnegative number specifying the bandwidth in density estimation. If h is NULL, the algorithm attempts to find h in a neighborhood centered at either the AMISE bandwidth or ROT bandwidth (see htype).
##' @param htype character string specifying the method used to calculate a reference bandwidth for the density estimation. htype is ignored if h is given. Valid options of are "ROT" and "AMISE" (see details).
##' @param nclust integer, or a vector of integers specifying the pool of the number of clusters in automatic variation. The default is 2:10.
##' @param ac integer indicating which automatic cut method is used. This is ignored if centroids = 'user'. The valid options are:
##' \itemize{
##' \item{ac = 1: }{centroids are chosen to be the data points x's with the largest delta values such that f(x) >= a'th percentile of all f(x). The number of centroids is given by the parameter nclust. The cutting percentile(s) is given by the parameter f.cut. }
##' \item{ac = 2: }{let l denote the straight line connecting (min(f), max(delta)) and (max(f), min(delta)). The centroids are selected to be data points above l and farthest away from it. The number of centroids is given by the parameter nclust.}
##' }
##' @param f.cut number between (0, 1) or numeric vector of numbers between (0, 1). f.cut is used when centroids = "auto" and ac = 1 to automatically select cluster centroids from the decision plot (see ac). The default is c(0.1, 0.2, 0.3).
##' @param fdelta character string that specifies the method used to estimate local density f(x) at each data point x. The default (recommended) is "mnorm" that uses a multivariate Gaussian density estimation to calculate f. Other options are listed below. Here 'distm' denotes the distance matrix. 
##' \itemize{
##' \item{unorm}{(f <- 1/(h * sqrt(2 * pi)) * rowSums(exp(-(distm/h)^2/2))); Univariate Gaussian smoother}
##' \item{weighted}{(f <- rowSums(exp(-(distm/h)^2))); Univariate weighted smoother}
##' \item{count}{(f <- rowSums(distm < h) - 1); Histogram estimator (used in Rodriguez [2014])}
##' }
##' @param dmethod character string that is passed to the 'method' argument in function dist(), which is used to calculate the distance matrix if 'distm' is not given. The default is "euclidean".
##' @param draw boolean. If draw = TRUE the clustering result is plotted after the algorithm finishes. The plot is produced by by plot.adpclust(ans), where 'ans' is the outcome of 'adpclust()'
##' @return An 'adpclust' object that contains the list of the following items.
##' \itemize{
##' \item{clusters}{ Cluster assignments. A vector of the same length as the number of observations.}
##' \item{centers:}{ Indices of the clustering centers.}
##' \item{silhouette:}{ Silhouette score from the final clustering result.}
##' \item{nclust:}{ Number of clusters.}
##' \item{h:}{ Final bandwidth.}
##' \item{f:}{ Final density vector f(x).}
##' \item{delta:}{ Final delta vector delta(x).}
##' \item{selection.type:}{ 'user' or 'auto'.}
##' }
##'
##' @references 
##' \itemize{
##' \item{GitHub: \url{https://github.com/ethanyxu/ADPclust}}
##' \item{Xiao-Feng Wang, and Yifan Xu, (2015) "Fast Clustering Using Adaptive Density Peak Detection." Statistical Methods in Medical Research, doi:10.1177/0962280215609948. }
##' \item{PubMed: \url{http://www.ncbi.nlm.nih.gov/pubmed/26475830}}
##' }
##' @export
##' @examples 
##' # Load a data set with 3 clusters
##' data(clust3)
##' 
##' # Automatically select cluster centroids
##' ans <- adpclust(clust3, centroids = "auto", draw = FALSE)
##' summary(ans)
##' plot(ans)
##'
##' # Specify distm instead of data
##' distm <- FindDistm(clust3, normalize = TRUE)
##' ans.distm <- adpclust(distm = distm, p = 2, centroids = "auto", draw = FALSE)
##' identical(ans, ans.distm)
##' 
##' # Specify the grid of h and nclust
##' ans <- adpclust(clust3, centroids = "auto", h = c(0.1, 0.2, 0.3), nclust = 2:6)
##'
##' # Specify that bandwidths should be searched around
##' # Wand's Asymptotic-Mean-Integrated-Squared-Error bandwidth
##' # Also test 3 to 6 clusters.
##' ans <- adpclust(clust3, centroids = "auto", htype = "AMISE", nclust = 3:6)
##' 
##' # Set a specific bandwidth value.
##' ans <- adpclust(clust3, centroids = "auto", h = 5)
##'
##' # Change method of automatic selection of centers
##' ans <- adpclust(clust3, centroids = "auto", nclust = 2:6, ac = 2)
##' 
##' # Specify that the single "ROT" bandwidth value by
##' # using the 'ROT()' function
##' ans <- adpclust(clust3, centroids = "auto", h = ROT(clust3))
##'
##' # Centroids selected by user
##' \dontrun{
##' ans <- adpclust(clust3, centroids = "user", h = ROT(clust3))
##' }
##'
##' # A larger data set
##' data(clust5)
##' ans <- adpclust(clust5, centroids = "auto", htype = "ROT", nclust = 3:5)
##' summary(ans)
##' plot(ans)

adpclust <- function(x = NULL,
                     distm = NULL,
                     p = NULL,
                     centroids = 'auto',
                     h = NULL, 
                     htype = 'amise',
                     nclust = 2:10,
                     ac = 1,
                     f.cut = c(0.1, 0.2, 0.3),
                     fdelta = 'mnorm',
                     dmethod = 'euclidean',
                     draw = FALSE
                     ){
    # -------------------------------------------------------------------------
    # Check arguments
    # -------------------------------------------------------------------------    
    if(!centroids %in% c('user', 'auto')){
        stop('arg centroids must be one of c(\'user\', \'auto\') Got ', centroids)  
    }
    if(!is.null(h)){
        if(!is.numeric(h)) stop('arg h must be numeric. Got ', class(h))
        if(length(h) == 0) stop('arg h is empty: ', h)    
        if(min(h) <= 0) stop('arg h must be nonnegative. Got', h)
    }    
    if(!tolower(htype) %in% c('amise', 'rot')){
        stop('arg centroids must be one of c(\'amise\', \'rot\') Got ', htype)  
    }
    if(!all(nclust == floor(nclust))) stop('arg nclust must all be integers. Got ', nclust)
    if(min(nclust) <= 1) stop('arg nclust must be integers > 1. Got ', nclust)
    if(!ac %in% c(1,2)) stop('arg ac must be one of c(1,2). Got ', ac)
    if(!is.numeric(f.cut)) stop('arg f.cut must be numeric. Got ', class(f.cut))
    if(length(f.cut) == 0) stop('arg f.cut is empty: ', f.cut)
    if(min(f.cut) < 0) stop('arg f.cut must be between 0 - 1. Got', f.cut)
    if(max(f.cut) >= 1) stop('arg f.cut must be between 0 - 1. Got', f.cut)    
    if(!fdelta %in% c('mnorm', 'unorm', 'weighted', 'count')){
        stop('arg fdelta must be one of c(\'mnorm\', \'unorm\', \'weighted\', \'count\'). Got ', fdelta)        
    } 
    if(is.null(x)){
        # Use distm
        if(is.null(distm)) stop("Must provide one of x or distm")
        if(!inherits(distm, 'dist')) stop("arg distm must inherit dist class. Got ", class(distm))
        if(is.null(p) && is.null(h)){
            stop("Bandwidth h and data x are not given. Must provide p to calculate h.")
        }
    }else{
        # Use x. Calculate distm.
        if(fdelta == "mnorm"){
            distm <- FindDistm(x, normalize = TRUE, method = "euclidean")
        }else{
            distm <- FindDistm(x, normalize = FALSE, method = dmethod)
        }
        p = ncol(x)
    }
    
    # -------------------------------------------------------------------------
    # Find bandwidth h
    # -------------------------------------------------------------------------    
    if(is.null(h)){
        if(fdelta != "mnorm"){
            stop("Must give h unless fdelta == 'mnorm'")
        }
        h <- FindH(p, attr(distm, 'Size'), htype)
    }

    # -------------------------------------------------------------------------    
    # Clustering with the 'user' option
    # -------------------------------------------------------------------------    
    if(centroids == "user"){
        if(length(h) > 1){
            stop("h must be a scalar when centroids == 'user'")
        }
        fd <- FindFD(distm, h, fdelta)
        ans <- FindClustersManual(distm, fd$f, fd$delta)
        ans[['h']] <- h
        ans[['f']] <- fd[['f']]
        ans[['delta']] <- fd[['delta']]
        ans[['selection.type']] <- 'user'
        class(ans) <- c("adpclust", "list")
        if(draw) plot.adpclust((ans))
        return(ans)
    }

    # -------------------------------------------------------------------------    
    # Clustring with the 'auto' option
    # -------------------------------------------------------------------------    
    if(centroids == "auto"){
        if(length(h) > 1){
            h.seq <- h
        }else{
            h.seq <- seq(h / 3, h * 3, length.out = 10)
        }
        # Find f and delta for each h
        fd.list <- lapply(h.seq, function(h) FindFD(distm, h, fdelta))
        result.list <- lapply(fd.list, function(fd){
            FindClustersAuto(distm = distm, 
                             f = fd[['f']], 
                             delta = fd[['delta']], 
                             ac = ac,
                             nclust = nclust,
                             f.cut = f.cut)  
        })
        score.seq <- sapply(result.list, function(x) x$silhouette)
        iwinner <- which.max(score.seq)
        # Generate a list of all tested possibilities
        tested <- list()
        for(i in seq_along(h.seq)){
            for(one.sil in result.list[[i]]$tested.sils){
                one.tested <- list(f.cut = attr(one.sil, 'f.cut'),
                                   f.cut.value = attr(one.sil, 'f.cut.value'),
                                   nclust = attr(one.sil, 'nclust'),
                                   h = h.seq[i],
                                   sil = as.vector(one.sil))
                tested <- c(tested, list(one.tested))                
            }
        }

        ans <- result.list[[iwinner]]
        ans[['tested.sils']] <- NULL # Redundant. In 'tested'
        ans[['h']] <- h.seq[iwinner]
        fd <- fd.list[[iwinner]]
        ans[['f']] <- fd[['f']]
        ans[['delta']] <- fd[['delta']]
        ans[['selection.type']] <- 'auto'
        ans[['tested']] <- tested
        class(ans) <- c("adpclust", "list")
        if(draw) plot.adpclust((ans))
        return(ans)
    }
    stop('centroids not recognized') # should never reach here.
}

