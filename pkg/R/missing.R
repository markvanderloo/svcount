#' Count missing values like a BOSS
#' 
#' @name svcount-package
#' @docType package
#' @useDynLib svcount
#' @import methods
#' 
{}


#' Count missing values like a BOSS
#'
#' @param x an R object
#' @param ... optional extra parameters.
#' @param by  Index in \code{dim(x)} that defines the dimension of the output.
#'
#' @section Details:
#' 
#' For objects with \code{storage.mode} \code{double}, both \code{NaN} and \code{NA} are considered
#' missing, similar to how \code{is.na} treats these values. The function \code{\link{count_NA}} counts
#' pure \code{NA}'s. Note that since \code{R >= 3.0}, the result of \code{NA + NaN} may be \code{NA}
#' or \code{NaN} (it depends on the compiler used to build R) and very likely \code{NA + NaN != NaN + NA}.
#'
#'
#' @export
#' @return Missing value counts.
#'
setGeneric("count_missing", function(x,...) standardGeneric("count_missing"))

#' @rdname count_missing
setMethod("count_missing","numeric", function(x,...){
  .Call('count_double_missing',x)  
})

#' @rdname count_missing
setMethod("count_missing","integer",function(x,...){
  .Call('count_integer_missing',x)
})

#' @rdname count_missing
setMethod("count_missing","character",function(x,...){
  .Call('count_character_missing',x)
})

#' @rdname count_missing
setMethod("count_missing","logical",function(x,...){
  .Call('count_integer_missing',x)  
})

# 'raw' has no NA definition, but since is.na works on it nevertheless, we present:
#' @rdname count_missing
setMethod("count_missing","raw",function(x,...) 0 )

#' @rdname count_missing
setMethod("count_missing","factor",function(x,...){
  .Call('count_int_missing',x)
})

#' @rdname count_missing
setMethod("count_missing","data.frame", function(x,...){
  sapply(x,count_missing)
})

# @rdname count_missing
# setMethod("count_missing","matrix", function(x,by=0,...){
#   switch(as.character(by)
#    , '0' = getMethod('count_missing', signature(storage2class[storage.mode(x)]))(x)
#    , '1' = setNames(.Call(matfun(x),x,dim(x),TRUE), rownames(x))
#    , '2' = setNames(.Call(matfun(x),x,dim(x),FALSE), colnames(x))
#   )
# })


#' @rdname count_missing
setMethod("count_missing","matrix",function(x, by=0,...){
  getMethod("count_missing","array")(x,by,...)
})


# get deployment class
dpclass <- function(x){
  s <- storage.mode(x)
  if ( s == "double" ) "numeric" else s
}  
  
arrfun <- function(x){
 sprintf("count_array_%s_missing",storage.mode(x))
}


#' @rdname count_missing
setMethod("count_missing","array",function(x,by,...){
  by <- as.integer(by)
  if ( identical(by,0L) )
    return( getMethod("count_missing",dpclass(x))(x) )

  outdim <- dim(x)[by]
  if (anyNA(outdim)) stop("Invalid output dimension")
  out <- array(0.0, dim=outdim, dimnames=dimnames(x)[by])
  .Call(arrfun(x),x,by,out)
  out
})


#' @rdname count_missing
setGeneric("count_NA",function(x,...) standardGeneric("count_NA"))

setMethod("count_NA","numeric",function(x,...){
  .Call("count_double_NA",x)
})




