
#define USE_R_INTERNALS
#include <R.h>
#include <Rinternals.h>
#include "sv.h"
#ifdef _OPENMP
#include <omp.h>
#endif

/* ---------------- integer ---------------- */

SEXP count_matrix_integer_row_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0]
    , ncol = INTEGER(dim)[1];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, nrow));

  int *X = INTEGER(x);
  double  *count = REAL(ans), *start = REAL(ans);  
  
  for ( idx i=0; i<nrow; i++, count++) (*count) = 0.0;
  count = start;
  
  for ( idx i=0; i < ncol; i++, count = start){
    for(idx j=0; j < nrow; j++, count++, X++){
      if ( *X == NA_INTEGER ) (*count)++;
    }
  }

  UNPROTECT(2);
  return ans;
}

SEXP count_matrix_integer_col_missing(SEXP x, SEXP nthrd){
  PROTECT(x);
  PROTECT(nthrd); 

  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0]
    , ncol = INTEGER(dim)[1]
    , nthreads = INTEGER(nthrd)[0];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, ncol));

  int *X = INTEGER(x);
  double *count = REAL(ans);
  
  for ( idx i=0; i < ncol; i++, count++ ) (*count) = 0.0;
  count = REAL(ans);
  
  #pragma omp parallel for num_threads(nthreads) 
  for ( idx j=0; j < ncol; j++ ){
    idx J = j*nrow;
    for ( idx i=0; i<nrow; i++){
      if ( X[i+J] == NA_INTEGER ) count[j]++;
    }
  }

  UNPROTECT(3);
  return ans;
}

/* ---------------- double ----------------- */

SEXP count_matrix_double_row_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0]
    , ncol = INTEGER(dim)[1];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, nrow));

  double *X = REAL(x);
  double  *count = REAL(ans), *start = REAL(ans);  
  
  for ( idx i=0; i < nrow; i++, count++ ) (*count) = 0.0;
  count = REAL(ans);
  
  for ( idx i=0; i < ncol; i++, count=start ){
    for(idx j=0; j < nrow; j++, count++, X++){
      if ( ISNAN(*X) ) (*count)++;
    }
  }

  UNPROTECT(2);
  return ans;
}


SEXP count_matrix_double_col_missing(SEXP x, SEXP nthrd){
  PROTECT(x);
  PROTECT(nthrd); 

  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0] 
    , ncol = INTEGER(dim)[1]
    , nthreads = INTEGER(nthrd)[0];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, ncol));

  double *X = REAL(x);
  double  *count = REAL(ans);  
  
  for ( idx i=0; i < ncol; i++, count++) (*count) = 0.0;
  count = REAL(ans);
  
  #pragma omp parallel for num_threads(nthreads) 
  for ( idx j=0; j < ncol; j++ ){
    idx J = j*nrow;
    for ( idx i=0; i < nrow; i++){
      if ( ISNAN(X[i + J]) ) count[j] += 1;
    }
  }

  UNPROTECT(3);
  return ans;
}

/* ---------------- character -------------- */

SEXP count_matrix_character_row_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0]
    , ncol = INTEGER(dim)[1];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, nrow));

  idx t = 0;
  double  *count = REAL(ans), *start = REAL(ans);
  
  for ( idx i=0; i < nrow; i++, count++ ) (*count) = 0.0;
  count = start;
  
  for ( idx i=0; i < ncol; i++, count=start ){
    for(idx j=0; j < nrow; j++, count++, t++){
      if ( STRING_ELT(x,t) == NA_STRING ) (*count)++;
    }
  }

  UNPROTECT(2);
  return ans;
}



SEXP count_matrix_character_col_missing(SEXP x, SEXP nthrd){
  PROTECT(x);
  PROTECT(nthrd);
  

  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0]
    , ncol = INTEGER(dim)[1]
    , nthreads = INTEGER(nthrd)[0]; 

  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, ncol));

  double  *count = REAL(ans);  

  for ( idx i=0; i < ncol; i++, count++) (*count) = 0.0;
  count = REAL(ans);

  
  
  #pragma omp parallel for num_threads(nthreads) 
  for ( idx j=0; j < ncol; j++ ){
    idx J = j*nrow;
    for ( idx i=0; i < nrow; i++ ){
      if ( STRING_ELT(x,i + J ) == NA_STRING ) count[j] += 1;
    }
  }
  UNPROTECT(3);
  return ans;
}





