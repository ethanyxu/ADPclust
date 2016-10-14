library(ADPclust)
context("Test FindFD function")

test_that("FindFD output", {
    x <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
    distm <- dist(x)
    for(fdelta in c('unorm', 'weighted', 'count', 'mnorm')){
        a <- ADPclust::FindFD(distm, h=3, fdelta = fdelta)
        expect_is(a, 'list')
        expect_equal(length(a), 2)
        expect_equal(names(a), c("f", "delta"))
        expect_is(a$f, 'numeric')
        expect_is(a$delta, 'numeric')
        
        expect_equal(length(a$f), length(a$delta))
    }
})

test_that("FindFD input", {
    expect_error(ADPclust::FindFD(matrix(1:9,3,3)))    
    expect_error(ADPclust::FindFD(matrix(1:9,3,3), 2, 'abc'))    
})
