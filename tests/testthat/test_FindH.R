library(ADPclust)
context("Test FindH function")

test_that("FindH output", {
    p = 10
    n = 5
    a <- FindH(p, n, htype = 'amise')
    expect_is(a, 'numeric')
    expect_equal(length(a), 1)
    a <- FindH(p, n, htype = 'AmIsE')
    expect_is(a, 'numeric')
    expect_equal(length(a), 1)            
    a <- FindH(p, n, htype = 'ROT')
    expect_is(a, 'numeric')
    expect_equal(length(a), 1)
    a <- FindH(p, n, htype = 'rot')
    expect_is(a, 'numeric')
    expect_equal(length(a), 1)    
})

test_that("FindFD input", {
    expect_error(ADPclust::FindH('abc', 4, 'amise'))    
    expect_error(ADPclust::FindH(10, 5, 'amisse'))    
})
