/* These functions are designed to simulate responses to
 * -- the binary response model (brm) and
 * -- the graded response model (grm) (and maybe others)
 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <math.h>

/* Define getDims to make it way easier to get the dimensions of matrices */
#define getDims( A ) INTEGER( coerceVector( getAttrib( A, R_DimSymbol ), INTSXP ) )


/* FIRST THE FUNCTION TO SIMULATE THE BINOMIAL MODEL VALUES */
SEXP brm( SEXP thetas, SEXP params )        /* the person and item parameters */
{

/* 1) create scalars in C to hold temporary number */
  int n_ppl, n_it;                          /* for the item and person dimensions */

  double p_exp;                             /* for the exponent of the probability */
  double p;                                 /* for the estimated probabilities of response */
  double u;                                 /* for the simulated uniform, random number */

  int i, j;                                 /* for the loop iteration */

/* 2)
 *    a) digest the datastructures from R into C */
  int *dimPar;
  double *pthet, *ppar;                    /* pointers to theta and params */

/*    b) get the dimensions of thet and param */
  n_ppl = length( thetas );

  dimPar = getDims( params );
  n_it = dimPar[ 0 ];

/*    c) protect the R objects */
  PROTECT( thetas = coerceVector( thetas, REALSXP ) );
  PROTECT( params = coerceVector( params, REALSXP ) );

/*    d) point to the R objects */
  pthet = REAL( thetas ); ppar = REAL( params );

/* 3)
 *    a) create sim to hold the answer */
  SEXP sim;
  double *psim;                          /* a pointer to sim */

/*    b) make sure to allocate space for the matrix */
  PROTECT( sim = allocMatrix( REALSXP, n_ppl, n_it ) );
  psim = REAL( sim );

/* get the RNG (random number) state from R */
  GetRNGstate();

/* for each person/item combination we need to simulate a response */
  for( i = 0; i < n_ppl; i++ )
    for( j = 0; j < n_it; j++ ){

/* Note (IMPORTANT) - it fills in by COLUMNS, just like the default in R:
 * par[ j , p - 1 ] = par[ (p - 1) * n_it + j ] (where p is 1 -- 3)
 * sim[ i , j ] = sim[ j * n_ppl + i ] (where j is 0 -- n_it - 1)
 * misc[ i , j ] = misc[ j * nrows + i ] (where j is 0 -- ncol - 1)
 */

/* calculating the probability of response (a = 1; b = 2; c = 3) */
      p_exp = exp( ppar[ 0 * n_it + j ] * ( pthet[ i ] - ppar[ 1 * n_it + j ] ) );
      p = ppar[ 2 * n_it + j ] + ( 1 - ppar[ 2 * n_it + j ] ) * p_exp / ( 1 + p_exp );

/* simulating a uniform random deviate */
      u = unif_rand();

/* if the uniform random deviate is below the probability, item correct */
      if( u <= p )
	psim[ j * n_ppl + i ] = 1;

/* if the uniform deviate is not below the probability, item incorrect :( */
      else
	psim[ j * n_ppl + i ] = 0;

    }

/* put the RNG (random number) state back to R */
  PutRNGstate();

/* wrap up and return the result to R */
  UNPROTECT( 3 );

  return( sim );

}


/* SECOND THE FUNCTION TO SIMULATE THE GRADED RESPONSE MODEL VALUES */
SEXP grm( SEXP thetas, SEXP params )        /* the person and item parameters */
{

/* 1) create scalars in C to hold temporary number */
  int n_ppl, n_it, n_bnd;                   /* for the item, person, and bound dimensions */

  double p_exp;                             /* for the exponent of the probability */
  double p;                                 /* for the estimated probabilities of responding above a category */
  double u;                                 /* for the simulated uniform, random number */

  int i, j, k;                              /* for the loop iteration, where k is the bound */

/* 2)
 *    a) digest the datastructures from R into C */
  int *dimPar;
  double *pthet, *ppar;                    /* pointers to theta and params */

/*    b) get the dimensions of thet and param (including the bounds) */
  n_ppl = length( thetas );

  dimPar = getDims( params );
  n_it = dimPar[ 0 ];

  n_bnd = dimPar[ 1 ] - 1;

/*    c) protect the R objects */
  PROTECT( thetas = coerceVector( thetas, REALSXP ) );
  PROTECT( params = coerceVector( params, REALSXP ) );

/*    d) point to the R objects */
  pthet = REAL( thetas ); ppar = REAL( params );

/* 3)
 *    a) create sim to hold the answer */
  SEXP sim;
  double *psim;                           /* a pointer to sim */

/*    b) make sure to allocate space for the matrix */
  PROTECT( sim = allocMatrix( REALSXP, n_ppl, n_it ) );
  psim = REAL( sim );

/* get the RNG (random number) state from R */
  GetRNGstate();

/* for each person/item combination we need to simulate a response */
  for( i = 0; i < n_ppl; i++ )
    for( j = 0; j < n_it; j++ ){

/* simulating a uniform random deviate before calculating probabilities */
      u = unif_rand();

/* start out assuming that they responded in the lowest category */
      psim[ j * n_ppl + i ] = 1;
      
      for( k = 1; k <= n_bnd; k++ ){      /* k starts at 1 because 1 is the place of the FIRST b */

/* Note (IMPORTANT) - it fills in by COLUMNS, just like the default in R:
 * par[ j , p - 1 ] = par[ (p - 1) * n_it + j ] (where p is 1 -- n_bnd + 1)
 * sim[ i , j ] = sim[ j * n_ppl + i ] (where j is 0 -- n_it - 1)
 * misc[ i , j ] = misc[ j * nrows + i ] (where j is 0 -- ncol - 1)
 */

/* calculating the probability of responding above that category (a = 1; b1 = 2; b2 = 3; ... ; bK = n_bnd) */
	p_exp = exp( ppar[ 0 * n_it + j ] * ( pthet[ i ] - ppar[ k * n_it + j ] ) );
	p = p_exp / ( 1 + p_exp );

/* if the uniform random deviate is below the probability, respond in the next category */
	if( u <= p )
	    psim[ j * n_ppl + i ] = k + 1;

      }

    }

/* put the RNG (random number) state back to R */
  PutRNGstate();

/* wrap up and return the result to R */
  UNPROTECT( 3 );

  return( sim );

}
