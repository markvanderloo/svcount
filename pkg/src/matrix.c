#include <R.h>
#include <Rinternals.h>

typedef size_t idx;


SEXP count_matrix_integer_row_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0] - 1;
 
  idx l = length(x);

  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, nrow+1));

  int *X = INTEGER(x);
  double  *count = REAL(ans);  
  
  for ( idx i=0; i<nrow; i++, count++) (*count) = 0.0;
  count = REAL(ans);
  
  idx iout = 0;
  for ( idx i = 0; i < l; i++, X++){
    if ( *X == NA_INTEGER ) ++count[iout]; 
    if (iout < nrow) ++iout; else iout = 0;
  }

  UNPROTECT(2);
  return ans;
}


SEXP count_matrix_integer_col_missing(SEXP x){
  PROTECT(x);
 
  SEXP dim = getAttrib(x,R_DimSymbol);
  int nrow = INTEGER(dim)[0] - 1
    , ncol = INTEGER(dim)[1];
 
//  idx l = length(x);

  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, ncol));

  int *X = INTEGER(x);
  double  *count = REAL(ans);  
  
  for ( idx i=0; i<ncol; i++, count++) (*count) = 0.0;
  count = REAL(ans);
  
  for ( idx j=0; j < ncol; j++){
    for ( idx i=0; i<=nrow; i++, X++){
      if ( *X==NA_INTEGER) ++count[j];
    }
  }

  UNPROTECT(2);
  return ans;
}







