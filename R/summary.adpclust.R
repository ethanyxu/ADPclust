##' Summarizes the result from the adpclust() function.
##'
##' @title Summary of adpclust
##' @param object object of class "adpclust" that is returned from adpclust().
##' @param ... other arguments. NOT used. 
##' @return NULL
##' @export

summary.adpclust <- function(object, ...){
    cat("-- ADPclust Result -- \n\n")
    cat("Number of obs.: \t", length(object[['clusters']]), "\n")
    cat("Centroids selection: \t", object[['selection.type']], "\n")
    cat("Number of clusters: \t", length(object[['centers']]), "\n")
    cat("Avg. Silhouette: \t", object[['silhouette']], "\n")
    cat("Elements in result: \t", paste0(names(object)), sep = " $")
    cat("\nf(x): \n")
    print(summary(object[['f']]))
    cat("\ndelta(x): \n")
    print(summary(object[['delta']]))

    invisible(NULL)
}

