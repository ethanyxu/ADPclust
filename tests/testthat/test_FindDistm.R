library(ADPclust)
context("Test FindDistm function")

test_that("FindDistm output", {
    x <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
    for(normalize in c(TRUE, FALSE)){
        a <- ADPclust::FindDistm(x, normalize = normalize, 'euclidean')
        expect_is(a, 'dist')
    }
})

test_that("FindFD input", {
    expect_error(ADPclust::FindDistm('abc', TRUE))    
    expect_error(ADPclust::FindDistm(data.frame(x = 1:10, y = 1:10), TRUE, method = 'amisse'))    
    expect_error(ADPclust::FindDistm(data.frame(), FALSE, method = 'amisse'))        
    
})
