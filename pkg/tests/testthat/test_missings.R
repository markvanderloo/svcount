
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

context("Counting in matrices")

# testing per type/direction since C-implementations are type/direction-specific.

test_that("counting in integer matrices", {
  expect_equal(count_missing(matrix(0L,nrow=2,ncol=2)),0)
  expect_equal(count_missing(matrix(c(0L,0L,NA,0L,0L,0L),nrow=2,ncol=3)),1)
  expect_equal(count_missing(matrix(c(0L,0L,NA,0L,0L,0L),nrow=2,ncol=3),by=1), c(1,0))
  expect_equal(count_missing(matrix(c(0L,0L,NA,0L,0L,0L),nrow=2,ncol=3),by=2), c(0,1,0))
  expect_equal(count_missing(matrix(c(0L,0L,NA,0L,0L,0L),nrow=2,ncol=3),by=c(1,2)),array(c(0,0,1,0,0,0),dim=c(2,3)))
})

test_that("counting in numeric matrices", {
  expect_equal(count_missing(matrix(0L,nrow=2,ncol=2)),0)
  expect_equal(count_missing(matrix(c(0.1,0.2,NA,0.3,0.4,0.5),nrow=2,ncol=3)),1)
  expect_equal(count_missing(matrix(c(0.1,0.2,NA,0.3,0.4,0.5),nrow=2,ncol=3),by=1), c(1,0))
  expect_equal(count_missing(matrix(c(0.1,0.2,NA,0.3,0.4,0.5),nrow=2,ncol=3),by=2), c(0,1,0))
  expect_equal(count_missing(matrix(c(0.1,0.2,NaN,0.3,0.4,0.5),nrow=2,ncol=3)),1)
  expect_equal(count_missing(matrix(c(0.1,0.2,NaN,0.3,0.4,0.5),nrow=2,ncol=3),by=1),c(1,0))
  expect_equal(count_missing(matrix(c(0.1,0.2,NaN,0.3,0.4,0.5),nrow=2,ncol=3),by=2),c(0,1,0))
})

test_that("counting in character matrices", {
  expect_equal(count_missing(matrix("a",nrow=2,ncol=2)),0)
  expect_equal(count_missing(matrix(c("a","b",NA,"c","d","e"),nrow=2,ncol=3)), 1)
  expect_equal(count_missing(matrix(c("a","b",NA,"c","d","e"),nrow=2,ncol=3), by=1), c(1,0))
  expect_equal(count_missing(matrix(c("a","b",NA,"c","d","e"),nrow=2,ncol=3), by=2), c(0,1,0))
})

context("Counting in multidimensional arrays")

# testing per type since C-implementations are type-specific.

test_that("counting over integer arrays",{
  a <- array(1:12,dim=c(2,2,3))
  expect_equivalent(count_missing(a), 0) 
  a[1:6] <- NA
  expect_equivalent(count_missing(a,by=0),6)
  expect_equivalent(count_missing(a,by=1),array(c(3,3),dim=2))
  expect_equivalent(count_missing(a,by=2),array(c(4,2),dim=2))
  expect_equivalent(count_missing(a,by=3),array(c(4,2,0),dim=3))
  expect_equivalent(count_missing(a,by=c(1,2)),array(c(2,2,1,1),dim=c(2,2)))
  expect_equivalent(count_missing(a,by=c(1,3)),array(c(2,2,1,1,0,0),dim=c(2,3)))
  expect_equivalent(count_missing(a,by=c(2,3)),array(c(2,2,2,0,0,0),dim=c(2,3)))
})

test_that("counting over numeric arrays",{
  a <- array(1:12,dim=c(2,2,3))/2
  expect_equivalent(count_missing(a), 0) 
  a[1:6] <- NA
  expect_equivalent(count_missing(a,by=0),6)
  expect_equivalent(count_missing(a,by=1),array(c(3,3),dim=2))
  expect_equivalent(count_missing(a,by=2),array(c(4,2),dim=2))
  expect_equivalent(count_missing(a,by=3),array(c(4,2,0),dim=3))
  expect_equivalent(count_missing(a,by=c(1,2)),array(c(2,2,1,1),dim=c(2,2)))
  expect_equivalent(count_missing(a,by=c(1,3)),array(c(2,2,1,1,0,0),dim=c(2,3)))
  expect_equivalent(count_missing(a,by=c(2,3)),array(c(2,2,2,0,0,0),dim=c(2,3)))
})


test_that("counting over character arrays",{
  a <- array(1:12,dim=c(2,2,3))/2
  expect_equivalent(count_missing(a), 0) 
  a[1:6] <- NA
  expect_equivalent(count_missing(a,by=0),6)
  expect_equivalent(count_missing(a,by=1),array(c(3,3),dim=2))
  expect_equivalent(count_missing(a,by=2),array(c(4,2),dim=2))
  expect_equivalent(count_missing(a,by=3),array(c(4,2,0),dim=3))
  expect_equivalent(count_missing(a,by=c(1,2)),array(c(2,2,1,1),dim=c(2,2)))
  expect_equivalent(count_missing(a,by=c(1,3)),array(c(2,2,1,1,0,0),dim=c(2,3)))
  expect_equivalent(count_missing(a,by=c(2,3)),array(c(2,2,2,0,0,0),dim=c(2,3)))
})

context("Counting over data.frames")
test_that("data.frames are counted over correctly",{
  d <- data.frame(
      int = c(NA,2L,3L)
    , dbl = c(1.1,NA,3.3)
    , chr = c('a','b',NA)
  )
  expect_equal(count_missing(d),3)
  expect_equal(count_missing(d,by=1),c(1,1,1))
  expect_equal(count_missing(d,by=2),c(int=1,dbl=1,chr=1))
})


