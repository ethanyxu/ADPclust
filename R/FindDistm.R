##' Find distance matrix from data
##'
##' @title Find distance matrix from data
##' @param x data
##' @param normalize boolean. normalize data before calculating distance?
##' @param method passed to 'dist()'
##' @export
##' @return distance matrix of class dist
##' @author Ethan Xu
FindDistm <- function(x, normalize, method = 'euclidean'){
    if(!inherits(x, 'data.frame') && !inherits(x, 'matrix')) stop('arg x must be data frame or matrix')
    if(nrow(x) == 0) stop('x is empty. Cannot calculate distance matrix.')
    if(!inherits(normalize, 'logical')) stop('arg normalize must be boolean')
    if(normalize){
        ## distm.std <- as.matrix(dist(scale(dat, center = FALSE, scale = TRUE),
        ##                             method = "euclidean"))
        sds <- apply(x, 2, stats::sd)
        distm <- stats::dist(scale(x, center = FALSE, scale = sds), method = method, upper = TRUE)
    }else{
        distm <- stats::dist(x, upper = TRUE, method = method)
    }
    return(distm)
}
