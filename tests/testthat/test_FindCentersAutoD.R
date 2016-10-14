library(ADPclust)
context("Test FindCentersAutoD")

test_that("Testing FindCentersAutoD output format", {
    load("data_fd.RData")
    a <- FindCentersAutoD(f, delta, nclust = 3)
    expect_is(a, 'list')
})

test_that("Testing FindCentersAutoD output values",{
    load("data_fd.RData") # load sample f and delta
    a <- FindCentersAutoD(f, delta, nclust = 3)
    expect_equal(setequal(a[[1]], c(100, 80, 18)), TRUE)
    a <- FindCentersAutoD(f, delta, nclust = 1:4)
    for(i in 1:4){
        expect_equal(setequal(a[[i]], c(100, 80, 18, 16)[1:i]), TRUE)
    }
    a <- FindCentersAutoD(f, delta, nclust = c(2,3,3,3,5,5))
    expect_equal(length(a), 3)
    expect_equal(setequal(a[[1]], c(100, 80)), TRUE)
    expect_equal(setequal(a[[2]], c(100, 80, 18)), TRUE)
    expect_equal(setequal(a[[3]], c(100, 80, 18, 16, 98)), TRUE)
    expect_equal(length(a), 3) 
})