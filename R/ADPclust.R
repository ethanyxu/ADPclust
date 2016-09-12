##' @title Fast Clustering Using Adaptive Density Peak Detection
##' 
##' @description Clustering of data by finding cluster centers from estimated density peaks. It is a non-iterative procedure that incorporates multivariate Gaussian density estimation. The number of clusters as well as bandwidths can either be selected by the user or selected automatically through an internal clustering criterion.
##' 
##' @details Given n data points in p dimensions, adpclust() first finds local density estimation f(x) of each data point x. The bandwidth h used in density estimation can either be explicitly specified by user through the argument 'h', or automatically selected from a range of testing values. In the case of automatic selection of bandwidths, first a reference bandwidth h0 is calculated by one of the two methods: Scott's Rule-of-Thumb value (htype = "ROT") or Wand's Asymptotic-Mean-Integrated-Squared-Error value (htype = "AMISE"), then 10 values equally spread in the range [1/3h0, 3h0] are tested. 
##'
##' For each data point x, adpclust() also finds an 'isolation' index delta(x), which is defined as the distance between x and the closest point y with f(y) > f(x). The scatter plot (f(x), delta(x)) is called the decision plot. For an appropriate h, cluster centroids appear in the upper-right corner of the decision plot, i.e. points with large f(x) and delta(x). After centroids are picked from the decision plot either by user (centroids = 'user') or automatically (centroids = "auto"), other data points are clustered to the cluster marked by the closest centroid. 
##'
##' When centroids = 'user', the decision plot is generated and displayed on screen. The user selects centroids by clicking the points on the upper right corner of the decision plot. A right click or ESC ends the selection.
##'
##' @param x numeric data frame where rows are observations and columns are variables.
##' @param distm distance matrix of class 'dist'. Ignored if x is given.
##' @param p number of variables (ncol(x)). Ignored if x is given.
##' @param centroids character string specifying "user" or "auto" selection of cluster centroids.
##' @param h nonnegative number specifying the bandwidth in density estimation NULL (default). If h is NULL, the algorithm will attempt to find h in a neighborhood centered at either the AMISE bandwidth or ROT bandwidth (see htype).
##' @param htype character string specifying the method used to calculate a reference bandwidth for the density estimation. 'htype' is ignored if h is not NULL. Currently the two possible choices of 'htype' are "ROT" and "AMISE" (see details).
##' @param nclust integer, or integer vector specifying the pool of the number of clusters in automatic variation. Default is 2:10.
##' @param ac integer indicating which automatic cut method is used. Currently it takes one of two values:
##' \itemize{
##' \item{ac = 1: }{in the f vs. delta decision plot, 'nclust' points with f > percentile f.cut and nclust largest delta's are declaired centroids.}
##' \item{ac = 2: }{in the f vs. delta decision plot, denote by l the diagonal line connecting the point with smallest f and largest delta, and the point with largest f and smallest delta. 'nclust' points that are above l, and have are farthest away from l are declared centroids.}
##' }
##' @param f.cut number between (0, 1) or numeric vector of numbers between (0,1). f.cut is used in centroids = "auto" to automatically select cluster centroids from the decision plot. Points with f(x) > f.cut and high delta(x) are selected as one set of candidate centroids (see details). Default = c(0.1, 0.2, 0.3).
##' @param fdelta character string that specifies the method to estimate densities at each data point. The default (recommended) is "mnorm": multivariate Gaussian density estimation. Other options include
##' \itemize{
##' \item{unorm}{(f <- 1/(h * sqrt(2 * pi)) * rowSums(exp(-(distm/h)^2/2))); Univariate Gaussian smoother}
##' \item{weighted}{(rho <- rowSums(exp(-(distm/h)^2))); Univariate weighted smoother}
##' \item{count}{(rho <- rowSums(distm < h) - 1); Histogram estimator (used in Rodriguez [2014])}
##' }
##' @param dmethod character string describing distance measures used in dist() to calculate proximity matrix of dat. This is passed to the argument "method" in dist(). Default = "euclidean"
##' @param verbose if TRUE progress will be displayed.
##' @param draw if TRUE results will be plotted on screen. Same as plot.adpclust(ans), where 'ans' is the outcome of 'adpclust()'
##' @return An 'adpclust' object, which contains the list of the following items.
##' \itemize{
##' \item{clusters}{ Cluster assignments.}
##' \item{centers:}{ Indices of the clustering centers.}
##' \item{silhouette:}{ Silhouette score.}
##' \item{nclust:}{ Number of clusters.}
##' \item{h:}{ Final bandwidth.}
##' \item{f:}{ Final density vector f(x) for each data point.}
##' \item{delta:}{ Final delta vector delta(x) for each data point.}
##' \item{selection.type:}{ 'user' or 'auto'}
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

adpclust <- function(x = NULL, # data matrix
                     distm = NULL, # distance matrix. ignored if x is given
                     p = NULL, # ignored if x is given
                     centroids = 'auto', # [user, auto]
                     h = NULL, # bandwidth
                     htype = 'amise', # [amise, rot] methods to calculate h (ignored if h is not null)
                     nclust = 2:10, # number of clusters
                     ac = 1, # [1,2] automatic cutting method
                     f.cut = c(0.1, 0.2, 0.3), # cutting percentile for cutting method 1. ignored if ac == 2
                     fdelta = 'mnorm', # [mnorm, unorm, weighted, count] methods to calculate fdelta
                     dmethod = 'euclidean', # methods to calculate distance matrix. ignored if distm != null.
                     verbose = FALSE,
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
    # Clustering with ‘user’ option
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
    # Clustring with ‘auto’ option
    # -------------------------------------------------------------------------    
    if(centroids == "auto"){
        if(length(h) > 1){
            h.seq <- h
        }else{
            h.seq <- seq(h / 3, h * 3, length.out = 10)
        }
        fd.seq <- lapply(h.seq, function(h) FindFD(distm, h, fdelta))
        result.seq <- lapply(fd.seq, function(fd){
            FindClustersAuto(distm = distm, 
                             f = fd[['f']], 
                             delta = fd[['delta']], 
                             ac = ac,
                             nclust = nclust,
                             f.cut = f.cut)  
        })
        score.seq <- sapply(result.seq, function(x) x$silhouette)
        iwinner <- which.max(score.seq)
        ans <- result.seq[[iwinner]]
        ans[['h']] <- h.seq[iwinner]
        fd <- fd.seq[[iwinner]]
        ans[['f']] <- fd[['f']]
        ans[['delta']] <- fd[['delta']]
        ans[['selection.type']] <- 'auto'
        class(ans) <- c("adpclust", "list")
        if(draw) plot.adpclust((ans))        
        return(ans)
    }
    stop('centroids not recognized') # should never reach here.
}

