
#define USE_R_INTERNALS
#include <R.h>
#include <Rinternals.h>
#include "sv.h"
#include <omp.h>

SEXP get_max_threads(){
  
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = omp_get_max_threads(); 

  UNPROTECT(1);
  return ans;
}

