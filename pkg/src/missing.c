
#include <R.h>
#include <Rinternals.h>

typedef unsigned long long int idx;

// --- simple counters over vectors ---//

SEXP mkans(double x){
    SEXP ans;
    ans = PROTECT(allocVector(REALSXP, 1));
    REAL(ans)[0] = x;
    UNPROTECT(1);
    return ans;
}

SEXP count_double_missing(SEXP x){
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


// --- counters over matrices --- //

static void update_mat_counters(idx *irow, idx *icol, idx *iout, idx n, int byrow){
  if ( *irow == n-1 ){ 
    *irow = 0; 
    *(icol)++;
  } else { 
    (*irow)++;
  }
  (*iout) = byrow ? *irow : *icol;
}


SEXP count_matrix_double_missing(SEXP x, SEXP dim, SEXP byrow){
  PROTECT(x);
  PROTECT(dim);
  PROTECT(byrow);
  
  idx l = (idx) length(x)
    , n = (idx) INTEGER(byrow)[0] ? INTEGER(dim)[0] : INTEGER(dim)[1]; // output size
  int by_row = INTEGER(byrow)[0];

  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, n));

  double *X = REAL(x)
    , *count = REAL(ans);  
  
  for ( idx i=0; i<n; i++) count[i] = 0.0;
  
  idx iout = 0, irow=0, icol = 0;
  for ( idx i = 0; i < l; i++, X++){
    if ( ISNA(*X) ) count[iout] += 1;
    update_mat_counters(&irow, &icol, &iout, n, by_row);
  }

  UNPROTECT(4);
  return ans;

}


SEXP count_matrix_integer_missing(SEXP x, SEXP dim, SEXP byrow){
  PROTECT(x);
  PROTECT(dim);
  PROTECT(byrow);
  
  idx l = length(x)
    , n = INTEGER(byrow)[0] ? INTEGER(dim)[0] : INTEGER(dim)[1]; // output size
  int by_row = INTEGER(byrow)[0];

  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, n));

  int *X = INTEGER(x);
  double  *count = REAL(ans);  
  
  for ( idx i=0; i<n; i++) count[i] = 0.0;
  
  idx iout = 0, irow=0, icol=0;
  for ( idx i = 0; i < l; i++, X++){
    if ( *X == NA_INTEGER )  ++count[iout]; 
    update_mat_counters(&irow, &icol, &iout, n, by_row);
  }

  UNPROTECT(4);
  return ans;
}


SEXP count_matrix_character_missing(SEXP x, SEXP dim, SEXP byrow){
  PROTECT(x);
  PROTECT(dim);
  PROTECT(byrow);
  
  idx l = length(x)
    , n = INTEGER(byrow)[0] ? INTEGER(dim)[0] : INTEGER(dim)[1]; // output size
  int by_row = INTEGER(byrow)[0];

  SEXP ans;
  ans = PROTECT(allocVector(REALSXP, n));

  double  *count = REAL(ans);  
  
  for ( idx i=0; i<n; i++) count[i] = 0.0;
  
  idx iout = 0, irow=0, icol=0;
  for ( idx i = 0; i < l; i++){
    if ( STRING_ELT(x,i) == NA_STRING )  ++count[iout]; 
    update_mat_counters(&irow, &icol, &iout, n, by_row);
  }

  UNPROTECT(4);
  return ans;
}


// --- General counters, storing counts in a (possibly multidimensional) array ---//

// from single to multi-index, i -> I
static void vec2ten(idx i, int *dim, int ndim, idx *I){
  idx p = 1, m;

  for ( int j=0; j<ndim; j++){
    m = dim[j];
    I[j] = (i / p) % m;
    p *= m;
  }
}

// from multi to single index, I -> i
static int ten2vec(idx *I, int *dim, int ndim){
  idx i=0, p=1;
  for ( int j = 0; j < ndim; j++){
    i += I[j] * p;
    p *= dim[j];
  }
  return i;
}

/*
 * x    : input array
 * isub : integer vector subsetting dim(x)
 * count: output array: correct dimensions, initialized to 0.
 *
*/
SEXP count_array_integer_missing(SEXP x, SEXP isub, SEXP count){
  PROTECT(x);
  PROTECT(isub);
  PROTECT(count);

  int *X = INTEGER(x);
  idx l_in = (idx) length(x);
  SEXP dim_in = getAttrib(x,R_DimSymbol);
  int nd_in = (int) length(dim_in);
  int *d_in = INTEGER(dim_in);

  int nsub = length(isub);
  int *sub  = INTEGER(isub);
  // translate indices to base 0.
  for (int j=0; j<nsub; j++) sub[j]--;


  double *out = REAL(count); 
  idx l_out = (idx) length(count);

  SEXP dim_out = getAttrib(count, R_DimSymbol);
  int nd_out =  (int) length(dim_out);
  int *d_out = INTEGER(dim_out); 

  idx *Iin = (idx *) R_alloc(nd_in, sizeof(idx));
  idx *Iout = (idx *) R_alloc(nd_out, sizeof(idx));
  idx t;
  for (idx i = 0; i < l_in; i++, X++){
    if ( *X == NA_INTEGER ){
      // compute local multi-index in input array
      vec2ten(i, d_in, nd_in, Iin);
      // drop indices not in output array
      for (int j=0; j<nsub; j++) Iout[j] = Iin[sub[j]];
      // compute single index in output array
      t = ten2vec(Iout, d_out, nd_out);
      ++out[t];
    }
  }

  UNPROTECT(3);
  return R_NilValue;

}










