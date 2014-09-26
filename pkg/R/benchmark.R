
##################################################
# Benchmarking functions for the 'svcount package'
##################################################

gen_vector <- function(n=1e6, nNA=floor(n/2), vectype='numeric'){
  x <- do.call(vectype,list(n))
  x[sample(n,nNA,replace=FALSE)] <- NA
  x
}


#' benchmark counts over simple vectors
#' 
#' @param ns vector lengths (about half will be NA)
#' @param nthreads number of threads to use
#' @export
sv_benchmark_vector <- function(ns=10^(3:7),nthreads=get_max_threads()){

  types <- c('integer','numeric','character')
  out <- expand.grid(n=ns,type=types, stringsAsFactors=FALSE)
  out$base_R <- numeric(nrow(out))
  out$svcount <- numeric(nrow(out))

  for ( i in seq_len(nrow(out)) ){
    x <- gen_vector(n=out$n[i], vectype=out$type[i])
    bm <- as.data.frame(
        microbenchmark(times=50
          , svcount   = count_missing(x,nthreads=nthreads)
          , base_R     = sum(is.na(x))
        ))
    v <- aggregate(bm$time,by=list(bm$expr), median,na.rm=TRUE)
    out$base_R[i]  = v[v[,1]=="base_R",2]
    out$svcount[i] = v[v[,1]=="svcount",2]
  }
  out$ratio <- out$svcount / out$base_R

  out

}




