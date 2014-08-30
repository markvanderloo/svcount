
setGeneric("count_missing", function(x,...) standardGeneric("count_missing"))


setMethod("count_missing","numeric", function(x,...){
  .Call('count_double_missing',x)  
})


setMethod("count_missing","integer",function(x,...){
  .Call('count_integer_missing',x)
})

setMethod("count_missing","character",function(x,...){
  .Call('count_character_missing',x)
})

setMethod("count_missing","logical",function(x,...){
  .Call('count_integer_missing',x)  
})

# 'raw' has no NA definition, but since is.na works on it nevertheless, we present:
setMethod("count_missing","raw",function(x,...) 0 )

setMethod("count_missing","factor",function(x,...){
  .Call('count_int_missing',x)
})

setMethod("count_missing","data.frame", function(x,...){
  sapply(x,count_missing)
})

setMethod("count_missing","matrix",function(x,by=c('sum','row','col'),...){
  by <- match.arg(by)
  switch(by
   , 'sum' = getMethod('count_missing', signature(storage2class[storage.mode(x)]))(x)
   , 'row' = setNames(.Call(matfun(x),x,dim(x),TRUE), rownames(m))
   , 'col' = setNames(.Call(matfun(x),x,dim(x),FALSE), colnames(m))
  )
})

class2storage <- c(
  "numeric" = "double"
  ,"integer" = "integer"
  ,"factor" = "integer"
  ,"character" = "character"
)

storage2class <- setNames(names(class2storage),class2storage)

matfun <- function(x){
  sprintf("count_matrix_%s_missing",storage.mode(x))
}

setMethod("count_missing","array",function(x,by,...){
  by <- as.integer(by)
  outdim <- dim(x)[by]
  if (anyNA(outdim)) stop("Invalid output dimension")
  out <- array(0.0, dim=outdim, dimnames=dimnames(x)[by])
  .Call("count_array_integer_missing",x,by,out)
  out
})






