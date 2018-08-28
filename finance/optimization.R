
###########################################################################################
# Routine: satisfaction.simple
###########################################################################################

satisfaction.simple = function(w, sims, lambda = 1 ) {
  mean( sims %*% w ) - 0.5 * lambda * sd( sims %*% w );
};

###########################################################################################
# Routine: genetic.optimization.sim
###########################################################################################

genetic.optimization.sim <- function(
  sim.rtns,
  satisfaction,
  nBranches = 100,
  nTrials = 100,
  CrossoverRatio = 0.05,
  MutationFactor = 0.20,
  lambda = 1,
  precision = 1e-5
) {
  nAssets = ncol( sim.rtns );
  raw.weights = matrix( runif( nAssets * nBranches, 0, 1 ), ncol = nAssets, nrow = nBranches );
  weights = raw.weights / matrix( rep( apply( raw.weights, 1, sum ), nAssets ), ncol = nAssets, nrow = nBranches, byrow = F );

  S_max = rep( NaN, nTrials );

  t = 1
  while( t < nTrials ) {
     S = apply( weights, 1, function(x) satisfaction( x, sim.rtns, lambda ) );
     S_max[t] = max( S );
     kmax = which( S == max( S ) );
     best_weights = matrix( weights[ kmax, ], nrow = length( kmax ) );
     n_best_weights = length( kmax );

     Crossovers = matrix( runif( nAssets * nBranches, 0, 1 ), ncol = nAssets, nrow = nBranches );
     Crossovers = Crossovers <= .05;

     MutationLoc = matrix( sample( 1:nBranches, 3*nBranches, replace = TRUE ), ncol = 3, nrow = nBranches );

     new_weights = weights * NaN;
     for( i in 1:nBranches ) {
       new_weights[ i, ] = weights[ MutationLoc[i,1],] + CrossoverRatio*( weights[ MutationLoc[i,2], ] - weights[ MutationLoc[i,3], ] );
       new_weights[ i, Crossovers[i, ] ] = best_weights[ sample( 1:n_best_weights, 1 ), Crossovers[i, ] ];
     };

     all_weights = rbind( weights, new_weights );
     S_tot = apply( all_weights, 1, function(x) satisfaction( x, sim.rtns, lambda ) );
     best_locs = order( S_tot, decreasing = TRUE );

     weights = all_weights[ best_locs, ][ 1:nBranches, ];

     if( sd( S_tot ) < precision ) {
       break
     } else {
       t = t+1;
     };
  };

  Sat = satisfaction( weights[1, ], sim.rtns, lambda );
  output = list( Weights = weights[1, ], Iter = t, Satisfaction = Sat );
  return( output );
};

###########################################################################################
# Routine: genetic.optimization
###########################################################################################

genetic.optimization <- function( 
  rtn, 
  covar, 
  satisfaction, 
  nBranches = 100, 
  nTrials = 100, 
  CrossoverRatio = 0.05,
  MutationFactor = 0.20, 
  lambda = 1, 
  precision = 1e-5
) {
  nAssets = length( rtn );
  raw.weights = matrix( runif( nAssets * nBranches, 0, 1 ), ncol = nAssets, nrow = nBranches );
  weights = raw.weights / matrix( rep( apply( raw.weights, 1, sum ), nAssets ), ncol = nAssets, nrow = nBranches, byrow = F );

  S_max = rep( NaN, nTrials );

  t = 1
  while( t < nTrials ) {
     S = apply( weights, 1, function(x) satisfaction( x, rtn, covar, lambda ) );
     S_max[t] = max( S );
     kmax = which( S == max( S ) ); 
     best_weights = matrix( weights[ kmax, ], nrow = length( kmax ) );
     n_best_weights = length( kmax );

     Crossovers = matrix( runif( nAssets * nBranches, 0, 1 ), ncol = nAssets, nrow = nBranches );
     Crossovers = Crossovers <= .05;

     MutationLoc = matrix( sample( 1:nBranches, 3*nBranches, replace = TRUE ), ncol = 3, nrow = nBranches );

     new_weights = weights * NaN;
     for( i in 1:nBranches ) {
       new_weights[ i, ] = weights[ MutationLoc[i,1],] + CrossoverRatio*( weights[ MutationLoc[i,2], ] - weights[ MutationLoc[i,3], ] );
       new_weights[ i, Crossovers[i, ] ] = best_weights[ sample( 1:n_best_weights, 1 ), Crossovers[i, ] ];
     };

     all_weights = rbind( weights, new_weights );
     S_tot = apply( all_weights, 1, function(x) satisfaction( x, rtn, covar, lambda ) );
     best_locs = order( S_tot, decreasing = TRUE );
 
     weights = all_weights[ best_locs, ][ 1:nBranches, ];
    
     if( sd( S_tot ) < precision ) {
       break
     } else { 
       t = t+1;
     };
  };

  Sat = satisfaction( weights[1, ], rtn, covar, lambda );
  output = list( Weights = weights[1, ], Iter = t, Satisfaction = Sat );
  return( output );
};

###########################################################################################
# Routine: calc.resampled.portfolio
###########################################################################################

calc.resampled.portfolio <- function( 
  rtn, 
  covar, 
  nPeriods = 120, 
  nTrials = 100,
  num.points = 50, 
  constrain.budget = TRUE,
  constrain.noshort = TRUE,  
  constraints = NULL
) {
  nAssets = length( rtn );
  nObs = nPeriods * nTrials; 

  sim.ts = chol.generate( nObs, mu = rtn, covar = covar );

  sim.rtn = matrix( NaN, nrow = num.points, ncol = nTrials );
  sim.var = matrix( NaN, nrow = num.points, ncol = nTrials );
  sim.wts = matrix( 0,   nrow = num.points, ncol = nAssets );

  for( p in 1:nTrials ) {
    ts = sim.ts[ p:(p+nPeriods-1), ];
    est.cov = cov( ts );
    est.rtn = apply( ts, 2, mean );
   
    efr = calc.efficient.frontier( est.rtn, est.cov, num.points = num.points, 
			constrain.budget = constrain.budget, constrain.noshort = constrain.noshort, constraints = constraints );

    sim.rtn[, p ] = efr$Returns;
    sim.var[, p ] = efr$Variance; 
    sim.wts = sim.wts + efr$Weights / nTrials;
  };

  output = list( Returns = sim.rtn, Variance = sim.var, Weights = sim.wts );
};

###########################################################################################
# Routine: calc.efficient.frontier
###########################################################################################

calc.efficient.frontier <- function( 
  rtn, 
  covar, 
  num.points = 50, 
  constrain.budget = TRUE, 
  constrain.noshort = TRUE, 
  constraints = NULL
) {
  TOL = 1e-8;
  nAssets = length( rtn );

  variance = rep( NaN, num.points );
  weights  = matrix( NaN, nrow = num.points, ncol = nAssets );

  dvec = t( rep( 0, nAssets ) );
  Amat = c();
  bvec = c();

  if( constrain.budget ) {
    Amat = rbind( Amat, rbind( rep(-1, nAssets ), rep( 1, nAssets ) ) );
    bvec = c( bvec, -1-TOL, 1-TOL );
  };

  if( constrain.noshort ) {
    Amat = rbind( Amat, diag( 1, nAssets ) );
    bvec = c( bvec, rep( 0 - TOL, nAssets ) );
  };

  if( !is.null( constraints ) ) {
    Amat = rbind( Amat, constraints$Amat );
    bvec = c( bvec, constraints$bvec );
  };

  min.var = solve.QP( Dmat = covar, dvec = dvec, Amat = t( Amat ), bvec = bvec );
  min.rtn = min.var$solution %*% rtn;

  rtns = seq( from = min.rtn, to = max( rtn ), length.out = num.points );

  for( i in 1:length( rtns ) ) {
    R = rtns[i];
    
    A = rbind( Amat, rbind( -t(rtn), t(rtn ) ) );
    b = c( bvec,  -R-TOL, R-TOL );

    sol = solve.QP( Dmat = covar, dvec = dvec, Amat = t( A ), bvec = b );
    wts = sol$solution;

    weights[i,] = wts;
    variance[i] = wts %*% covar %*% wts;
  };

  output = list( Variance = variance, StdDev = sqrt( variance ), Returns = rtns, Weights = weights );
  return( output );
};

###########################################################################################
# Routine: opt.generate.ts
###########################################################################################

opt.generate.ts <- function( 
  nAssets, 
  nObs, 
  riskFreeRate = .01
) {
  mu_mean = .04;
  mu = rnorm( nAssets, mu_mean, mu_mean / 2 ) + riskFreeRate;

  SR_mean = .45;
  SR = rnorm( nAssets, SR_mean, SR_mean / 2 );

  noise = 0.0;
  sigma_raw = abs( mu / SR );
  sigma = sigma_raw + rnorm( nAssets, 0, noise * sigma_raw );

  C = matrix( rnorm( nAssets^2, 0.3, 0.5 ), ncol = nAssets );
  A = C %*% t(C);
  correl = diag( 1 / sqrt( diag( A )  ) ) %*% A %*% diag( 1 / sqrt( diag( A ) ) );
  covar = diag( sigma ) %*% correl %*% diag( sigma );
    
  ts = chol.generate( nObs, mu = mu, covar = covar );
  data = list( TS = ts, Correlation = correl, Std = sigma, Mean = mu, Covariance = covar );
  return( data ); 
};



