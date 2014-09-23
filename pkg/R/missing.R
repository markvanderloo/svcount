#' Count missing values like a BOSS
#' 
#' @name svcount-package
#' @docType package
#' @useDynLib svcount
#' @import methods
#' 
{}

#' get maximum number of available threads
#' 
#' @export
get_max_threads <- function() .Call("get_max_threads")


#' Count missing values like a BOSS
#'
#' @param x an R object
#' @param ... optional extra parameters.
#' @param by  Index in \code{dim(x)} that defines the dimension of the output.
#' @param nthread Number of threads to request (doesn't always mean the system gives them to ya).
#'
#' @section Details:
#' 
#' For objects with \code{storage.mode} \code{double}, both \code{NaN} and \code{NA} are considered
#' missing, similar to how \code{is.na} treats these values. Note that since \code{R >= 3.0}, the result 
#' of \code{NA + NaN} may be \code{NA} or \code{NaN} (it depends on the compiler implementation
#' used to build R) and very likely \code{NA + NaN != NaN + NA}.
#'
#'
#' @export
#' @return Missing value counts.
count_missing <- function(x,...){
  UseMethod("count_missing")  
}


#' @rdname count_missing
#' @export 
count_missing.default <- function(x,...){
  switch(storage.mode(x)
    , 'integer'   = .Call('count_integer_missing'  , x)
    , 'double'    = .Call('count_double_missing'   , x)
    , 'character' = .Call('count_character_missing', x)
  )
}


#' @rdname count_missing
#' @export 
count_missing.numeric <- function(x,...){
  .Call('count_double_missing',x)  
}

#' @rdname count_missing
#' @export 
count_missing.integer <- function(x,...){
  .Call('count_integer_missing',x)
}

#' @rdname count_missing
#' @export 
count_missing.character <- function(x,...){
  .Call('count_character_missing',x)
}

#' @rdname count_missing
#' @export 
count_missing.logical <- function(x,...){
  .Call('count_integer_missing',x)  
}

# 'raw' has no NA definition, but since is.na works on it nevertheless, we present:
#' @rdname count_missing
#' @export 
count_missing.raw <- function(x,...) 0 

#' @rdname count_missing
#' @export 
count_missing.factor <- function(x,...){
  .Call('count_integer_missing',x)
}

#' @rdname count_missing
#' @export 
count_missing.data.frame <- function(x,by=0,...){
  switch(paste0(by,collapse="")
    , "0" = sum(sapply(x,count_missing.default))
    , "1" = {
      out <- numeric(nrow(x))
      for ( i in seq_len(ncol(x))){
        switch(storage.mode(x[[i]])
          , 'integer'   = .Call('count_missing_along_integer'   , x[[i]],out)
          , 'double'    = .Call('count_missing_along_double'    , x[[i]],out)
          , 'character' = .Call('count_missing_along_character' , x[[i]],out)
        )
      }
      out
    }
    , "2" = sapply(x,count_missing.default)
  )
}


## Methods for matrices ----------------------------

# discover storage mode of an array or matrix and return as 
# the corresponding R class.
array_mode <- function(x){
  m <- storage.mode(x)
  if (m=='double') 'numeric' else m
}


#' @rdname count_missing
#' @export 
count_missing.matrix <- function(x, by=0, nthread=get_max_threads(),...){
  if (is.character(by)) by <- match_by(by,x)
  nthread <- as.integer(nthread)
  stopifnot(nthread > 0)
  switch(paste0(by,collapse="")
    , '0' = count_missing.default(x,...)
    , '1' = setNames( # ugly switch but literals are needed to pass R CMD CHECK --as-cran
        switch(storage.mode(x) 
          , 'integer'   = .Call('count_matrix_integer_row_missing'  , x, nthread)
          , 'double'    = .Call('count_matrix_double_row_missing'   , x, nthread)
          , 'character' = .Call('count_matrix_character_row_missing', x, nthread)
        ), rownames(x)
      )
    , '2' = setNames(
        switch(storage.mode(x)
          , 'integer'   = .Call('count_matrix_integer_col_missing'  , x, nthread)
          , 'double'    = .Call('count_matrix_double_col_missing'   , x, nthread)
          , 'character' = .Call('count_matrix_character_col_missing', x, nthread)
        ), colnames(x)
      )
    , '12'= {
        out <- array(0.0,dim=dim(x),dimnames=dimnames(x))
        switch(storage.mode(x)
          , 'integer'     = .Call('count_missing_along_integer'  , x, out)
          , 'double'      = .Call('count_missing_along_double'   , x, out)
          , 'character'   = .Call('count_missing_along_character', x, out)
        )
      } 
    , '21' = { 
      out <- array(0.0,dim=dim(x),dimnames=dimnames(x))
      t( count_missing.matrix(x, by=c(1,2),nthread=nthread,...) )
    }
    , stop('Invalid marginal definition\n')
  )
}

match_by <- function(by,x){
  if ( length(by) == 0 ){ 
    0
  } else {
    match(by,names(dimnames(x)))
  }
}


#' @rdname count_missing
#' @export 
count_missing.array <- function(x,by=0,...){
  by <- as.integer(by)
  if ( by == 0 )
    return( count_missing.default(x,...) )

  outdim <- dim(x)[by]
  outdn <- dimnames(x)[by]
  
  if (length(dim(x)) == 2 )
    return(array(
      count_missing.matrix(x,by,...)
      , dim = outdim
      , dimnames = outdn
    ))
  
  if (anyNA(outdim)) stop("Invalid output dimension")
  out <- array(0.0, dim=outdim, dimnames=outdn)
  switch(storage.mode(x)
    , 'integer'   = .Call("count_array_integer_missing",x,by,out)
    , 'double'    = .Call("count_array_double_missing", x,by,out)
    , "character" = .Call("count_array_character_missing", x,by,out)
  )
}






