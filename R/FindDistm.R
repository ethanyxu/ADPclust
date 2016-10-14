##' A wrapper of the dist() method, with the option to rescale the data with standard deviation of each dimension before calculating the distance matrix. NOTE: If fdelta='mnorm' is passed to adpclust(), then the distm is calculated from rescaled data internally, i.e. distm <- FindDistm(x, normalize = TRUE).
##'
##' @title Find the distance matrix from data.
##' @param x data
##' @param normalize boolean. Normalize data before calculating distance?
##' @param method passed to 'dist()'
##' @export
##' @return distance matrix of class dist.
##' @author Ethan Xu
FindDistm <- function(x, normalize = FALSE, method = 'euclidean'){
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
