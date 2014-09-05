
//#define USE_R_INTERNALS
#include <R.h>
#include <Rinternals.h>
#include "sv.h"

// --- simple counters over vectors ---//

static SEXP mkans(double x){
    SEXP ans;
    ans = PROTECT(allocVector(REALSXP, 1));
    REAL(ans)[0] = x;
    UNPROTECT(1);
    return ans;
}

// count any missings (NaN and NA)
SEXP count_double_missing(SEXP x){
  PROTECT(x);
  idx l = (idx) length(x);
  double *X = REAL(x);
  double n = 0;

  for ( idx i = 0; i < l; i++, X++ ){
    if ( ISNAN(*X) ) ++n;
  }
  UNPROTECT(1);
  return mkans(n);
}

// count pure NA (not NaN)
SEXP count_double_NA(SEXP x){
  PROTECT(x);
  idx l = (idx) length(x);
  double *X = REAL(x);
  double n = 0;

  for ( idx i = 0; i < l; i++, X++ ){
    if ( ISNA(*X) ) ++n;
  }
  UNPROTECT(1);
  return mkans(n);
}

SEXP count_integer_missing(SEXP x){
  PROTECT(x);
  idx  l = (idx) length(x);
  int *X = INTEGER(x);
  double n = 0;
  for( idx i = 0; i < l; i++, X++){
    if ( *X == NA_INTEGER ) ++n;
  }
  UNPROTECT(1);
  return mkans(n);
}

SEXP count_character_missing(SEXP x){
  PROTECT(x);
  idx  l = (idx) length(x);
  double n = 0;
  for( idx i = 0; i < l; i++ ){
    if ( STRING_ELT(x,i) == NA_STRING ) ++n;
  }
  UNPROTECT(1);
  return mkans(n);
}


// counting along a vector. 
// Useful for row counting over data.frames and some 
// edge cases in counting over arrays.

SEXP count_missing_along_integer(SEXP x, SEXP out){
  PROTECT(x);
  PROTECT(out);
  idx l = (idx) length(x);
  int *X = INTEGER(x);
  double *count = REAL(out);

  for ( idx i=0; i < l; i++, count++, X++){
    if ( (*X) == NA_INTEGER ) (*count)++;
  }

  UNPROTECT(2);
  return out;
}


SEXP count_missing_along_double(SEXP x, SEXP out){
  PROTECT(x);
  PROTECT(out);
  idx l = (idx) length(x);
  double *X = REAL(x)
       , *count = REAL(out);

  for ( idx i=0; i < l; i++, count++, X++){
    if ( ISNAN(*X) ) (*count)++;
  }

  UNPROTECT(2);
  return out;
}


SEXP count_missing_along_character(SEXP x, SEXP out){
  PROTECT(x);
  PROTECT(out);
  idx l = (idx) length(x);
  double *count = REAL(out);

  for ( idx i=0; i < l; i++, count++){
    if ( STRING_ELT(x,i) == NA_STRING ) (*count)++;
  }

  UNPROTECT(2);
  return out;
}



