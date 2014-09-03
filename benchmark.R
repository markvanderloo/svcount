
library(svcount)
library(compiler)
library(microbenchmark)


# test data
x <- matrix(runif(1e7),ncol=100,nrow=1e5)

x[1:(1e7/2)] <- NA

# standard R functions (compiled)
sumNA <- cmpfun(function(x) sum(is.na(x)))
rowNA <- cmpfun(function(x) rowSums(is.na(x)))


microbenchmark(
  count_missing(x)
  ,sumNA(x)
  ,times=10
)

microbenchmark(
  count_missing(x,by=1)
  ,rowNA(x)
  ,times=10
)


y <- runif(1e7)
microbenchmark(
  count_missing(y)
  ,sumNA(y)
  ,times=10
)



library(microbenchmark)
dyn.load('pkg/src/missing.so')
rowmis <- function(x) .Call('count_matrix_integer_row_missing',x)
colmis <- function(x) .Call('count_matrix_integer_col_missing',x)
y <- as.integer(1:1e7)
y[1:(1e7/2)] <- NA
y <- matrix(y,ncol=2)
u<-colmis(y)
#u
microbenchmark(
  rowmis(y)
  , rowSums(is.na(y))
  , times=500
  , control=list(warmup=100)
)



