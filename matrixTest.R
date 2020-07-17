## Simple test wrapper for the programming assignment
## author: mig


library(testthat)

source("cacheMatrix.R")

# create and return a random invertible matrix of dimension nxn
makeRandomInvertibleMatrix <- function(n = 2) {
  repeat {
    v <- sample(n^2, replace = TRUE)
    m <- matrix(v, nrow=n, ncol=n)
    if(det(m) != 0)
      return(m)
  }
}

context("cachesolve")

# Make sure that cachesolve actually returns the inverted matrix...
test_that("cachesolve inverts", {
  
  m <- makeRandomInvertibleMatrix(3)
  minv <- solve(m)
  
  mc <- makeCacheMatrix(m)
  minvc <- cacheSolve(mc)
  
  expect_equal(minv, minvc)
})

# Test that the cached value is returned
test_that("caching works", {
  m <- makeRandomInvertibleMatrix(6)

  mc <- makeCacheMatrix(m) 
  minvc <- cacheSolve(mc)
  
  msg <- tryCatch(minvc <- cacheSolve(mc), message = function(c) c[1])
  expect_equal(msg$message, "getting cached data\n")
})