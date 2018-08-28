
######################################################################################################
# Routine: cum.density
#
# returns the cumulative density of a vector of random numbers
######################################################################################################

cum.density <- function( rands, N = 1000 ) {
  R = as.matrix( rands );

  x = seq( min(R), max(R), len = N );
  q = ((0:(nrow(R)-1))+0.5)/nrow(R);

  y = matrix( NaN, nrow = N, ncol = ncol(R) );

  for( i in 1:ncol( rands ) ) {
    y[,i] = approx( sort( R[,i] ), q, x, yleft = 0, yright = 1 )$y;
  }
   
  return( list( x = x, y = y ) );
};

######################################################################################################
# Routine: adj.covar
#
# If covariance has negative eigenvalues, then set them to zero
######################################################################################################

cov.semi.def <- function( covar ) {
  eigs = eigen( covar );
  vals = pmax( eigs$values, 1e-12 )
  adj.covar = eigs$vectors %*% diag( vals ) %*% t( eigs$vectors )
  return( adj.covar )
};

######################################################################################################
# Routine: chol.generate.t
######################################################################################################

chol.generate.t <- function( 
  nObs, 
  correl, 
  mu = rep( 0, ncol( correl ) ), 
  df = rep( Inf, ncol( correl ) ), 
  ncp = rep( 0, ncol( correl ) ), 
  scale = rep( 1, ncol( correl ) )
) {
  eigenValues = eigen(covar)$values;
 
  sigma = diag( as.numeric( scale ) );
  covar = sigma %*% correl %*% sigma;

  if( min( eigenValues ) < -1e-4 ) {
    stop( 'Eigenvalues are very negative.  The input covariance is not properly conditioned.' );
  } else {
    adj.covar = cov.semi.def( covar );
  };

  nAssets = length( df );
  cholesky = chol( as.matrix( adj.covar ) );

  rands = matrix( NA, nrow = nObs, ncol = nAssets );
  for( c in 1:nAssets ) {
    rands[,c] = rt( nObs, df[c], ncp[c] );
  };

  demeaned.rtns = rands %*% cholesky;

  rtns = demeaned.rtns + matrix( rep( mu, nObs ), ncol = nAssets, byrow = TRUE );

  return( rtns );
};

######################################################################################################
# Routine: chol.generate
######################################################################################################

chol.generate <- function( 
  nObs, 
  correl, 
  mu = rep( 0, ncol( correl ) ), 
  std = rep( 1, ncol( correl ) )
) {
  if( ncol( correl ) == 1 ) {
    ts = matrix( rnorm( nObs, 0, 1 ), ncol = 1 ) * std + mu;
    return( ts );
  };

  covar = diag( as.numeric( std ) ) %*% correl %*% diag( as.numeric( std ) );

  eigenValues = eigen(covar)$values;

  if( min( eigenValues ) < -1e-4 ) {
    stop( 'Eigenvalues are very negative.  The input covariance is not properly conditioned.' );
  } else {
    adj.covar = cov.semi.def( covar );
  };

  nAssets = length( mu );
  cholesky = chol( as.matrix( adj.covar ) );

  rands = matrix( rnorm( nAssets * nObs, 0, 1 ), nrow = nObs );

  rtns = rands %*% cholesky;

  ts = rtns + matrix( rep( mu, nObs ), ncol = nAssets, byrow = TRUE );

  return( ts );
};

######################################################################################################
# Routine: is.empty
######################################################################################################

is.empty <- function( input ) {
  if( is.hash( input ) ) {
    # Use the overloaded is.empty function for objects of class 'hash'
    return( hash::is.empty(input) );
  } else if( class(input) %in% c( 'FormatDates', 'FormatValues', 'FormatString' ) ) {
    return( FALSE );
  } else if( is.data.frame( input ) && nrow( input ) == 0 ) {
    return( TRUE );
  } else if( length( input ) > 1 ) {
    return( FALSE );
  } else if( length( input ) == 0 ) {
    return( TRUE );
  } else if( all( is.na( input ) ) ) {
    return( TRUE );
  } else if( is.character( input ) && all( input == '' ) ) {
    return( TRUE );
  } else {
    return( FALSE );
  };
};

