library(ADPclust)
context("Test ADPclust")

IsSameClustering <- function(c1, c2){
    if(length(c1) != length(c2)) return(FALSE)
    c1.codes <- unique(c1)
    c2.codes <- unique(c2)
    if(!setequal(c1.codes, c2.codes)) return(FALSE)
    for(c1.code in c1.codes){
        one.cluster <- which(c1 == c1.code)
        pts.in.c2 = c2[one.cluster]
        r <- range(pts.in.c2)
        if(r[1] != r[2]) return(FALSE)
    }
    return(TRUE)
}

load("clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
#load("./tests/testthat/clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
test_that("ADPclust output", {
    ## Check auto clustering output
    a <- adpclust(distm = clust3.distm, p = 2)
    expect_equal(inherits(a, "adpclust"), TRUE)
    expect_equal(inherits(a, "list"), TRUE)
    expect_equal(setequal(names(a), 
                          c("clusters", "centers", "silhouette",
                            "nclust", "h", "f", "delta", "selection.type",
                            "tested")),
                 TRUE)
    ## Test cluster result
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)
    expect_equal(length(a[['centers']]), 3)
    expect_equal(setequal(a[['centers']], c(30, 49, 77)), TRUE)
    
    ## More clustering tests with different parameter settings
    expect_error(adpclust(distm = clust3.distm), 
                 "Bandwidth h and data x are not given. Must provide p to calculate h.", 
                 fixed = TRUE)       
    a <- adpclust(distm = clust3.distm, p = 2, ac = 2)
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)
    a <- adpclust(distm = clust3.distm, p = 2, htype = 'rot')
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)
    a <- adpclust(distm = clust3.distm, p = 2, nclust = 3)
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)    
    a <- adpclust(distm = clust3.distm, p = 2, f.cut = seq(0.05, 0.4, 0.01))
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)        
    a <- adpclust(distm = clust3.distm, p = 2, h = c(0.05, 0.1, 0.15, 0.2))
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)
    expect_error(adpclust(distm = clust3.distm, p = 2, fdelta = 'unorm'), 
                 "Must give h unless fdelta == 'mnorm'", fixed = TRUE)
    a <- adpclust(distm = clust3.distm, p = 2, h = 0.14, fdelta = 'unorm')
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE) 
    expect_error(adpclust(distm = clust3.distm, p = 2, fdelta = 'weighted'), 
                 "Must give h unless fdelta == 'mnorm'", fixed = TRUE)
    a <- adpclust(distm = clust3.distm, p = 2, h = 0.14, fdelta = 'weighted')
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)     
    expect_error(adpclust(distm = clust3.distm, p = 2, fdelta = 'count'), 
                 "Must give h unless fdelta == 'mnorm'", fixed = TRUE)
    a <- adpclust(distm = clust3.distm, p = 2, h = 0.14, fdelta = 'count')
    expect_equal(IsSameClustering(a$clusters, clust3.clusters), TRUE)         
})
