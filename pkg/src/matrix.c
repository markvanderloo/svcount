
//#define USE_R_INTERNALS
#include <R.h>
#include <Rinternals.h>
#include "sv.h"

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

SEXP count_matrix_integer_col_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0] - 1
    , ncol = INTEGER(dim)[1];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, ncol));

  int *X = INTEGER(x);
  double *count = REAL(ans);
  
  for ( idx i=0; i < ncol; i++, count++ ) (*count) = 0.0;
  count = REAL(ans);
  
  for ( idx j=0; j < ncol; j++, count++ ){
    for ( idx i=0; i<=nrow; i++, X++){
      if ( *X==NA_INTEGER) (*count)++;
    }
  }

  UNPROTECT(2);
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

SEXP count_matrix_double_col_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0] - 1
    , ncol = INTEGER(dim)[1];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, ncol));

  double *X = REAL(x);
  double  *count = REAL(ans);  
  
  for ( idx i=0; i < ncol; i++, count++) (*count) = 0.0;
  count = REAL(ans);
  
  for ( idx j=0; j < ncol; j++, count++ ){
    for ( idx i=0; i<=nrow; i++, X++){
      if ( ISNAN(*X) ) (*count)++;
    }
  }

  UNPROTECT(2);
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

SEXP count_matrix_character_col_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0] - 1
    , ncol = INTEGER(dim)[1];
 
  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, ncol));

  idx t = 0;
  double  *count = REAL(ans);  
  
  for ( idx i=0; i < ncol; i++, count++) (*count) = 0.0;
  count = REAL(ans);
  
  for ( idx j=0; j < ncol; j++, count++ ){
    for ( idx i=0; i<=nrow; i++, t++){
      if ( STRING_ELT(x,t) == NA_STRING ) (*count)++;
    }
  }

  UNPROTECT(2);
  return ans;
}






