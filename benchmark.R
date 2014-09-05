
library(svcount)
library(microbenchmark)

N <- 1e7
NROW <- 1000

cat("### Benchmarking integers ------------------\n")
x <- matrix(1:N,nrow=NROW)
x[1:floor(N/2)] <- NA

microbenchmark(times=50           
  , count_missing(x,by=1)
  , rowSums(is.na(x))
)

microbenchmark(times=50           
 , count_missing(x,by=2)
 , colSums(is.na(x))
)

cat("### Benchmarking doubles -------------------\n")
x <- matrix((1:N)/2,nrow=NROW)
x[1:floor(N/2)] <- NA

microbenchmark(times=50           
 , count_missing(x,by=1)
 , rowSums(is.na(x))
)

microbenchmark(times=50           
 , count_missing(x,by=2)
 , colSums(is.na(x))
)

cat("### Benchmarking characters ------------------\n")
x <- matrix(rep(letters[1],N),nrow=NROW)
x[1:floor(N/2)] <- NA
microbenchmark(times=50           
 , count_missing(x,by=1)
 , rowSums(is.na(x))
)

microbenchmark(times=50           
   , count_missing(x,by=2)
   , colSums(is.na(x))
)


cat("### Benchmarking data.frames ---------------\n")
d <- data.frame(
  x = 1:N
  ,y = (1:N)/2
  ,z = rep(letters[1],N)
)
microbenchmark(times=50
  , count_missing(d,1)
  , rowSums(is.na(d))
)

microbenchmark(times=50
  , count_missing(d,2)
  , colSums(is.na(d))
)




