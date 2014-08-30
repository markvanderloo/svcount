
context("counting in vectors")

test_that("counting works in integer vectors",{
  expect_equal(count_missing(1:3),0)
  expect_equal(count_missing(c(1L,NA)),1)
})

test_that("counting works in numeric vectors",{
  expect_equal(count_missing(1:3/2),0)
  expect_equal(count_missing(c(1:3/2,NA)),1)
  expect_equal(count_missing(c(1:3/2,NaN)),1)
  expect_equal(count_NA(c(1:3/2,NaN)),0)
  expect_equal(count_NA(c(1:3/2,NA)),1)
  
})

test_that("counting works in character vectors",{
  expect_equal(count_missing(letters[1:3]),0)
  expect_equal(count_missing(c(letters[1:3],NA)),1)
})

context("counting in matrices")

test_that("counting in integer matrices", {
  expect_equal(count_missing(matrix(0L,nrow=2,ncol=2)),0)
  expect_equal(count_missing(matrix(c(0L,0L,NA,0L,0L,0L),nrow=2,ncol=3)),1)
  expect_equal(count_missing(matrix(c(0L,0L,NA,0L,0L,0L),nrow=2,ncol=3),by='row'),c(1,0))
  expect_equal(count_missing(matrix(c(0L,0L,NA,0L,0L,0L),nrow=2,ncol=3),by='col'),c(0,1,0))
})

test_that("counting in numeric matrices", {
  expect_equal(count_missing(matrix(0L,nrow=2,ncol=2)),0)
  expect_equal(count_missing(matrix(c(0.1,0.2,NA,0.3,0.4,0.5),nrow=2,ncol=3)),1)
  expect_equal(count_missing(matrix(c(0.1,0.2,NA,0.3,0.4,0.5),nrow=2,ncol=3),by='row'),c(1,0))
  expect_equal(count_missing(matrix(c(0.1,0.2,NA,0.3,0.4,0.5),nrow=2,ncol=3),by='col'),c(0,1,0))
  expect_equal(count_missing(matrix(c(0.1,0.2,NaN,0.3,0.4,0.5),nrow=2,ncol=3)),1)
  expect_equal(count_missing(matrix(c(0.1,0.2,NaN,0.3,0.4,0.5),nrow=2,ncol=3),by='row'),c(1,0))
  expect_equal(count_missing(matrix(c(0.1,0.2,NaN,0.3,0.4,0.5),nrow=2,ncol=3),by='col'),c(0,1,0))
})

test_that("counting in character matrices", {
  expect_equal(count_missing(matrix("a",nrow=2,ncol=2)),0)
  expect_equal(count_missing(matrix(c("a","b",NA,"c","d","e"),nrow=2,ncol=3)), 1)
  expect_equal(count_missing(matrix(c("a","b",NA,"c","d","e"),nrow=2,ncol=3), by='row'), c(1,0))
  expect_equal(count_missing(matrix(c("a","b",NA,"c","d","e"),nrow=2,ncol=3), by='col'), c(0,1,0))
})
