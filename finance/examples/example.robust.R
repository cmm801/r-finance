library( 'calibrate' );
TOL = 1e-8;

axioma.example.2.1 <- function( ) {
  w_b = c( 0.5, 0.5 );
  rtns = c( .025, .024 );
  stdv = c( .0042, .0033 );
  rho = 0.7;
  covar  = diag( stdv ) %*% matrix( c( 1, rho, rho, 1 ), ncol = 2, nrow = 2 ) %*% diag( stdv );

  eigs = eigen( covar );
  Lambda = diag( eigs$values );
  W = eigs$vectors;

  cov_inv = solve( covar );
  cov_12 = W %*% sqrt( Lambda ) %*% t( W ); 

  variance = function(w, Sigma) as.numeric( t(w) %*% Sigma %*% w );

  var.limit = 1.1^2 * variance( w_b, covar ) 

  ui = rbind( c( -1, -1 ), c( 1, 1 ) );
  ci = c( -1 - TOL, 1 - TOL )

  wts = w_b + cov_inv %*% rtns *
			sqrt( 0.1 * variance( w_b, covar ) / variance( rtns, cov_inv ) );
  return( wts );
};

get.idzorak.cov <- function( ) {
  covar = read.table( 'examples/idzorak_covariance.csv', sep = ' ', stringsAsFactors = F, header = T )
  return( covar ) 
};

get.idzorak.rtn <- function() {
  rtn = read.table( 'examples/idzorak_returns.csv', sep = ' ', stringsAsFactors = F, header = T )
  return( rtn )
};

axioma.example.2.2 <- function( 
  nPeriods = 60
) {
  rawcov = get.idzorak.cov();
  AssetClasses = rawcov$AssetClass;
  nAssets = length( AssetClasses );

  covar = data.matrix( rawcov[,-1] );
  rtn = data.matrix( get.idzorak.rtn()[,-1] ) / 100;

  sim.ts = chol.generate( nPeriods, mu = rtn, covar = covar );
  est.rtn = apply( sim.ts, 2, mean );
  est.covar = cov( sim.ts );

  true.frontier   = calc.efficient.frontier( rtn, covar );
  est.frontier    = calc.efficient.frontier( est.rtn, est.covar );
  
  true.weights = true.frontier$Weights;
  est.weights  = est.frontier$Weights;
  
  M = max( abs( rtn ) );
  plot( est.frontier$Variance, est.frontier$Returns, col = 'red', type = 'l', ylim = c( -0.5*M, 1.5*M ),
									    xlim = c( 0, 1.1* max( diag( covar ) ) ) );
  lines( true.frontier$Variance, true.frontier$Returns, type = 'l' );
  lines( diag( est.weights %*% covar %*% t( est.weights ) ), est.weights %*% rtn, col = 'blue' );
  points( diag( covar ), rtn, pch = 3 )
  textxy( diag( covar ), rtn, AssetClasses );
  points( diag( covar ), est.rtn, pch = 1, col = 'red' )
  textxy( diag( covar ), est.rtn, AssetClasses, dcol = 'red' );
};

resample.example <- function( 
  nPeriods = 60,
  nTrials = 100
) {
  axioma.example.2.2( nPeriods = nPeriods );
  rtn = get.idzorak.rtn()[,-1]/100;
  covar = data.matrix( get.idzorak.cov()[,-1] );

  res = calc.resampled.portfolio( rtn, covar, nPeriods = nPeriods, nTrials = nTrials );
  # Plot Estimated Resampled Efficient Frontier

  lines( apply( res$Variance, 1, mean ), apply( res$Returns, 1, mean ), type = 'l', col = 'purple' )
  # Plot Actual Resampled Efficient Frontier
  wts = res$Weights;
  res.rtn = wts %*% rtn;
  res.var = apply( wts, 1, function(x) x %*% covar %*% x );

  lines( res.var, res.rtn, type = 'l', col = 'green' );
};


example.genetic.frontier <- function() {
  rtn = get.idzorak.rtn()[,-1]/100;
  covar = data.matrix( get.idzorak.cov()[,-1] );

  satisfaction = function(wts, rtn, covar, lambda ) as.numeric( wts %*% rtn ) - lambda / 2 * as.numeric( wts %*% covar %*% wts )
  lambdas = seq( from = .01, to = 2, len = 50 )^4;
  weights = matrix( NaN, ncol = length( rtn ), nrow = length( lambdas ) );
  for( k in 1:length( lambdas ) ) {
    weights[k, ]= genetic.optimization( rtn, covar, satisfaction = satisfaction, lambda = lambdas[k] )$Weights;
  };

  varc = diag( weights %*% covar %*% t( weights ) );
  rtns = weights %*% rtn;
  output = list( Weights = weights, Variance = varc, Returns = rtns );
  return( output );
};

example.max.sharpe <- function() {
  SIG = c( .15, .20 );
  COV = diag( SIG ) %*% matrix( c( 1, 1/2, 1/2, 1 ), nrow = 2 ) %*% diag( SIG );
  RTN = c( .04, .05 );

  satisfaction = function(wts, rtn, covar, lambda ) as.numeric( wts %*% rtn ) / sqrt(  as.numeric( wts %*% covar %*% wts ) );
  wts = matrix( NaN, nrow = 100, ncol = 2 );
  for( i in 1:100 ) {
    wts[i, ] = genetic.optimization( RTN, COV, satisfaction = satisfaction, lambda = 1, precision = 1e-6 )$Weights;
  }
  return( wts );
}


