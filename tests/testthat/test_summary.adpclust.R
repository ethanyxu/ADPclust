library(ADPclust)
context("Test summary")

load("clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
#load("./tests/testthat/clust3_test.RData") # clust3.{x, distm, h, f, delta, clusters}
test_that("summary adpclust", {
    ## Check auto clustering output
    a <- adpclust(distm = clust3.distm, p = 2)
    expect_equal(inherits(a, "adpclust"), TRUE)    
    summary(a)
})
