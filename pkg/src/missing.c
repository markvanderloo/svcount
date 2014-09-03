
#include <R.h>
#include <Rinternals.h>

typedef size_t idx;

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


SEXP count_array_double_missing(SEXP x, SEXP isub, SEXP count){
  PROTECT(x);
  PROTECT(isub);
  PROTECT(count);

  double *X = REAL(x);
  idx l_in = (idx) length(x);
  SEXP dim_in = getAttrib(x,R_DimSymbol);
  int nd_in = (int) length(dim_in);
  int *d_in = INTEGER(dim_in);

  int nsub = length(isub);
  int *sub  = INTEGER(isub);
  // translate indices to base 0.
  for (int j=0; j<nsub; j++) sub[j]--;


  double *out = REAL(count); 

  SEXP dim_out = getAttrib(count, R_DimSymbol);
  int nd_out =  (int) length(dim_out);
  int *d_out = INTEGER(dim_out); 

  idx *Iin = (idx *) R_alloc(nd_in, sizeof(idx));
  idx *Iout = (idx *) R_alloc(nd_out, sizeof(idx));
  idx t;
  for (idx i = 0; i < l_in; i++, X++){
    if ( ISNAN(*X) ){
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


SEXP count_array_character_missing(SEXP x, SEXP isub, SEXP count){
  PROTECT(x);
  PROTECT(isub);
  PROTECT(count);

  //double *X = REAL(x);
  idx l_in = (idx) length(x);
  SEXP dim_in = getAttrib(x,R_DimSymbol);
  int nd_in = (int) length(dim_in);
  int *d_in = INTEGER(dim_in);

  int nsub = length(isub);
  int *sub  = INTEGER(isub);
  // translate indices to base 0.
  for (int j=0; j<nsub; j++) sub[j]--;


  double *out = REAL(count); 

  SEXP dim_out = getAttrib(count, R_DimSymbol);
  int nd_out =  (int) length(dim_out);
  int *d_out = INTEGER(dim_out); 

  idx *Iin = (idx *) R_alloc(nd_in, sizeof(idx));
  idx *Iout = (idx *) R_alloc(nd_out, sizeof(idx));
  idx t;
  for (idx i = 0; i < l_in; i++){
    if ( STRING_ELT(x,i) == NA_STRING ){
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








