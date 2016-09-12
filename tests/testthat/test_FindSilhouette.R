library(ADPclust)
context("Test FindSilhouette")
test_that("Testing FindSilhouette", {
    load("2clust.RData") # distm, x, clusters
    a <- FindSilhouette(distm, clusters)
    expect_equal(round(a, 5), 0.92147)
})
