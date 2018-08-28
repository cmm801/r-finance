

##########################################################################################################
# Routine: calc.distribution.distance
#
# Calc the distance between two distibutions by integrating the difference in cumultive distributions
##########################################################################################################

calc.distrib.distance <- function( 
  true.dist, 
  theoretical.dist, 
  ZScore    = TRUE, 
  step.size = 1e-3
) {
  TOL = 1e-10;

  quantiles = ( 1:length( true.dist ) ) / length( true.dist );

  # Take the inverse cum of the theoretical dist wrt the true dist: F^{-1}_{true} ( F_{theory}(x) )
  FinvF = approx( sort( true.dist ), quantiles, sort( theoretical.dist ), yleft = 0, yright = 1 )$y;

  # Standardize the output so there is the same distance between each quantile
  dist = approx( FinvF, ( 1:length( FinvF ) ) / length( FinvF ), seq( from = 0, to = 1, by = step.size ), yleft = 0, yright = 1 )$y;

  R = ( sort( dist ) - ( 1:length( dist ) ) / length( dist ) );
  rsquare = sum( R * R ) / length( dist );
  
  if( ZScore == TRUE ) { 
    output = ZScore.distrib.distance( rsquare, true.dist = true.dist, theoretical.dist = theoretical.dist );
  } else {
    output = rsquare;
  };

  return( output );
};

##########################################################################################################
# Routine: ZScore.distrib.distance
#
# Calcuate the ZScore for the distance between distributions by finding the mean and standard deviation
# for two identical distributions with lengths equal to the input distributions.  
# Since the RSquared distance between distributions is lognormally distributed, we take the log of these parameters
##########################################################################################################

ZScore.distrib.distance <- function( x, true.dist, theoretical.dist ) {
  equal.dist = rep( NA, 100 );
  for( i in 1:100 ) {
     equal.dist[i] = calc.distrib.distance( rnorm( length( true.dist ), 0, 1 ), rnorm( length( theoretical.dist ), 0, 1 ), ZScore = FALSE );
   };

  output = ( log( x ) - mean( log( equal.dist ) ) ) / sd( log( equal.dist ) );
  
  return( output );
};

##########################################################################################################
# Routine: kl.divergence
#
# Kullback-Leibler divergence
# Calculate the difference between two probability distributions
##########################################################################################################

kl.divergence <- function( true.dist, theoretical.dist, step.size = 1e-3 ) {
  TOL = 1e-10;

  xleft  = min( true.dist, theoretical.dist );
  xright = max( true.dist, theoretical.dist );

  X = seq( from = xleft, to = xright, by = step.size );

  true.dens  = density( true.dist );
  theor.dens = density( theoretical.dist ); 

  P = approx( true.dens$x,  true.dens$y,  X,  yleft = TOL, yright = TOL )$y;
  Q = approx( theor.dens$x, theor.dens$y, X,  yleft = TOL, yright = TOL )$y;

  P = P / sum( P ); 
  Q = Q / sum( Q );
    
  KL = sum( P * log( P / Q ) );
  
  return( KL ); 
};


##########################################################################################################
# Routine: bootstrap.returns
#
# Generate a bootrapped time series of returns, given initial levels.
##########################################################################################################

bootstrap.returns  <- function( 
  levels, 
  nday.sim    = 21,  
  block.size  = 5, 
  num.sims    = 1e4
)
{
  output = c();

  for( nday in nday.sim ) {
    num.blocks 	 = floor( nday / block.size );
    remainder.size = nday %% block.size;

    block.rtns <- levels / lag( levels, block.size ) - 1;
    block.rtns <- matrix( as.numeric( block.rtns[ !is.na( rowSums( block.rtns ) ), ] ), ncol = ncol( levels ) );

    if( remainder.size != 0 ) {  
      remainder.rtns <- levels / lag( levels, remainder.size ) - 1;
      remainder.rtns <- as.matrix( remainder.rtns[ !is.na( rowSums( remainder.rtns ) ), ] );
    } else {
      remainder.rtns = 0 * levels;
    };

    rtn.elements = matrix( sample( 1:nrow( block.rtns ), num.sims * num.blocks, replace = TRUE ), ncol = num.blocks );

    sim.rtns = matrix( rep( NA, num.sims * ncol( levels ) ), ncol = ncol( levels ) );

    for( i in 1:ncol( levels ) ) {
      sim.rtns[ , i ] = apply( matrix( block.rtns[ rtn.elements, i ], nrow = num.sims ), 1, function(x) prod( 1 + x ) );
      sim.rtns[ , i ] = sim.rtns[, i] * ( 1 + remainder.rtns[ sample( 1:nrow(block.rtns), num.sims, replace = TRUE ), i ] );
    };

    output = rbind( output, t( sim.rtns - 1 ) );
  };
  
  return( output );
};

##########################################################################################################
# Routine: get.transition.matrix
#
# Given initial and final states, calculate the transition probability
##########################################################################################################

get.transition.matrix <- function( 
  initial, 
  final, 
  use.prior = TRUE
) {
  states = sort( unique( c( as.numeric( initial ), as.numeric( final ) ) ) );
  if( length( states ) == 1 )
     return( matrix( 1 ) );

  transition.matrix = matrix( NA, ncol = length( states ), nrow = length( states ) );

  for( i in 1:length( states ) ) {
     for( j in 1:length( states ) ) {
        transition.matrix[ i, j ] = sum( initial == states[i] & final == states[j], na.rm = TRUE );
     }
  };

  if( use.prior == TRUE )
    transition.matrix = transition.matrix + 1;

  transition.matrix = diag( 1 / rowSums( transition.matrix ) ) %*% transition.matrix;

  return( transition.matrix );
};

##########################################################################################################
# Routine: markov.bootstrap
#
# Given a vector of states and their transition probabilities, perform block bootstrap
##########################################################################################################

markov.bootstrap <- function( 
  all.states, 
  transition.matrix = c(), 
  nday.sim   = 20, 
  block.size = MARKOV_BLOCK_SIZE, 
  num.sims   = 1e4,
  initial.state = tail( all.states, 1 )
) {
  num.blocks = ceiling( nday.sim / block.size );
  
  if( identical( transition.matrix, c() ) ) {
    transition.prob = get.transition.matrix( all.states[ 1:length( all.states ) - 1 ], all.states[ 2:length( all.states ) ] );
  } else {
    transition.prob = transition.matrix;
  }; 
 
  state.vector = rep( initial.state, num.sims );  

  blocks = c();
  for( i in 1:num.blocks ) {
    tmp.state = evolve.markov( state.vector, transition.prob );
    tmp.locs  = state2location( all.states, tmp.state, block.size = block.size );     
    
    new.block = matrix( rep( tmp.locs, block.size ), nrow = block.size, byrow = TRUE ) + matrix( 0:(block.size-1), ncol = num.sims, nrow = block.size );
    blocks    = rbind( blocks, new.block ); 

    state.vector = all.states[ blocks[ nrow( blocks ), ] ];
  };  

  # Only keep the first nday.sim entries
  blocks = blocks[ 1:nday.sim, ];

  return( blocks );
};

##########################################################################################################
# Routine: find.vix.state
#
# From bucketing VXO, find the state on a given day (the asOfDate)
# The optional arguments 'start.date' and 'end.date' are used to define the time period against which the 
# value of the VIX on the asOfDate is compared.  The asOfDate need not lie within the start and end dates.
# If 'bucket.dates' is not empty, these are the dates on which the VIX will be evaluated for bucketing.
##########################################################################################################

find.vix.state <- function( 
  nBuckets = 10, 
  asOfDate = last.biz.date( today() ), 
  start.date = as.Date( '1986-01-01' ), 
  end.date   = max( asOfDate ), 
  bucket.dates = c() 
) {
   
  VXO = get.time.series( "^VXO", start.date = as.Date( '1986-01-01' ), end.date = max( c( asOfDate, bucket.dates ) ) );

  VXO = fill.missing( VXO, end.date = today() );

  bucket_vix = bucket.data( as.numeric( VXO ), num.buckets = nBuckets );
  
  state = bucket_vix[ match( asOfDate, as.Date( rownames( VXO ) ) ) ];    
  return( state );
};

##########################################################################################################
# Routine: markov.vix.bootstrap
#
# Perform a conditional markov bootstrap using the VIX value to represent the current state
##########################################################################################################

markov.vix.bootstrap <- function( 
  levels, 
  nday.sim      = 20, 
  block.size    = MARKOV_BLOCK_SIZE,  
  num.sims      = 1e4, 
  num.buckets   = 10, 
  output.all    = FALSE, 
  initial.state = -1, 
  min.bin.size  = 50,
  start.date    = as.Date( '1986-01-01' ), 
  end.date      = last.biz.date( today() - 1 )
) {
  VXO = get.time.series( "^VXO", start.date = start.date, end.date = end.date );
  clean.levels = levels[ !is.na( match( rownames( levels ), rownames( VXO ) ) ), ]

  nBuckets = num.buckets;
  adjust.bucket.size = TRUE;

  while( adjust.bucket.size == TRUE ) {
    bucket_vix = bucket.data( as.numeric( VXO ), num.buckets = nBuckets );

    # Match all TS dates with VXO dates, excluding the first TS date because we will be looking at daily returns
    all_states = bucket_vix[ match( rownames( clean.levels[-1,] ), rownames( VXO ) ) ];

    # If there are not enough examples of each state, then reduce the number of buckets and try again
    if( nBuckets == 1 || ( min( count( all_states )$freq ) >= min.bin.size && nrow( count( all_states ) ) == max( all_states ) ) )  {
      adjust.bucket.size = FALSE
    } else {
      nBuckets = nBuckets - 1;
    }; 
    if( nBuckets != num.buckets )
      warning( 'Used %d buckets instead of %d in markov.vix.bootstrap', nBuckets, num.buckets );
  };

  transition.matrix = get.transition.matrix( bucket_vix[-length( bucket_vix) ], bucket_vix[-1] );

  initial = ifelse( initial.state == -1, tail( all_states, 1 ), initial.state );

  blocks = markov.bootstrap( all_states, transition.matrix = transition.matrix, nday.sim = max( nday.sim ), 
							block.size = block.size, num.sims = num.sims, initial.state = initial );

  original.rtns = ri( clean.levels ); 

  sim.rtns   = matrix( original.rtns[ blocks ], ncol = ncol( blocks ), nrow = nrow( blocks ) );
  sim.levels = apply( 1 + sim.rtns, 2, cumprod )

  if( output.all ) {
    output = sim.levels; 
  } else {
    #  Add this bit of confusing logic so that we return a vector of zeros of nday.sim == 0
    output = sim.levels[ nday.sim + as.numeric( nday.sim == 0 ), ] * as.numeric( nday.sim != 0 );
  };

  output = matrix( output, ncol = num.sims, nrow = length( nday.sim ) ); 
  
  return( output );
};

##########################################################################################################
# Routine: bucket.data
#
# Bucket the input data into num.buckets different buckets
##########################################################################################################

bucket.data <- function( data, num.buckets = 10 ) {
  breakpoints = as.numeric( quantile( as.numeric( data ), ( 0:num.buckets ) / num.buckets, na.rm = TRUE ) );

  buckets = as.numeric( data ) * NaN;

  for( i in 1:num.buckets ) {
    buckets[ breakpoints[i] <= data & data <= breakpoints[i+1] ] = i;
  };

  return( buckets );
};

##########################################################################################################
# Routine: get.vix.transition.matrix
#
# Estimate the transition matrix between vol states from VXO (VIX) history
##########################################################################################################

get.vix.transition.matrix <- function() {
  VXO = as.numeric( get.time.series( "^VXO", start.date = as.Date( '1980-01-01' ) ) );

  bucket_vix = bucket.data( VXO, num.buckets = 10 );

  transition.matrix = get.transition.matrix( bucket_vix[-length( bucket_vix) ], bucket_vix[-1] );

  return( transition.matrix );
};

##########################################################################################################
# Routine: evolve.markov
#
# Given a vector of states and their transition probabilities, evolve the process to the next state.
##########################################################################################################

evolve.markov <- function( state, transition.prob ) {

  new.state = state * NaN; 

  for( i in 1:nrow( transition.prob ) ) {
     s_i = which( state == i );
     new.state[ s_i ] = ceiling( approx( cumsum( c( 0, transition.prob[ i, ] ) ), 0:nrow( transition.prob ), runif( length( s_i ), 0, 1 ) )$y );
  };
  
  return( new.state )
};

##########################################################################################################
# Routine: state2location
#
# Given a vector of states and a new state, find the locations of the new states
##########################################################################################################

state2location <- function( 
  in.states, 
  out.state, 
  block.size = MARKOV_BLOCK_SIZE
) {
  num.states = length( unique( c( in.states, out.state ) ) );

  new.locs = out.state * NaN;

  for( i in 1:num.states ) {
    s_i = which( out.state == i );
    new.locs[ s_i ] = sample( which( in.states[ 1:( length( in.states ) - block.size + 1 ) ] == i ), length( s_i ), replace = TRUE );
  };
 
  return( new.locs );
};

##########################################################################################################
# Routine: cum.prob.dist
#
# Given time series levels, calculate the cumulative probability distribution
##########################################################################################################

cum.prob.dist <- function( 
  levels, 
  mktvals, 
  nday.sim   = 21, 
  block.size = 5, 
  num.sims   = 1e4
)
{
  bootstrap.rtns = bootstrap.returns( levels, nday.sim = nday.sim, block.size = block.size, num.sims = num.sims );
  bootstrap.rtns = bootstrap.rtns[ , colnames( mktvals ) ];

  mv.chgs = ( (1 + bootstrap.rtns ) %*% diag( as.numeric( mktvals ) ) ) / sum( mktvals );
  mv.chgs = -1 + rowSums( mv.chgs, na.rm = TRUE );
  return( mv.chgs )
};


##########################################################################################################
# Routine: plot.cum.prob.dist
#
# Plot the cumulative distribution function of a portfolio
##########################################################################################################

plot.cum.prob.dist <- function( 
  levels,
  mktvals,
  nday.sim   = 21,
  block.size = 5,
  num.sims   = 1e4
)
{
  mv.chgs = cum.prob.dist( levels, mktvals, nday.sim = nday.sim, block.size = block.size, num.sims = num.sims );
  sorted.mv.chgs = sort( mv.chgs, decreasing=FALSE );

  plot( sorted.mv.chgs, ( 1:num.sims ) / num.sims, ylim = c( 0, 1 ) );
  for( h in seq( 0, 1, 0.1 ) )
    abline( h = h, lty = 'dotted' );

  from = round( 100 * min( sorted.mv.chgs ) ) / 100;
  to   = round( 100 * max( sorted.mv.chgs ) ) / 100;
  for( v in seq( from=from, to=to, by=0.02 ) ) 
    abline( v = v );
};

##########################################################################################################
# Routine: calc.VaR 
#
# Given time series levels and weights, calculate the portfolio VaR
##########################################################################################################

calc.VaR <- function( 
  levels,
  mktvals,
  VaR.pct    = c(),
  nday.sim   = 21,
  block.size = MARKOV_BLOCK_SIZE,
  num.sims   = 1e4, 
  method     = 'Saved Blocks', 
  initial.state = -1
)
{
  MV = c();

  state = ifelse( initial.state == -1, find.vix.state(), initial.state );
  for( i in 1:ncol( levels ) ) {
    lev = levels[ !is.na( levels[, i ] ), ][,i];

    if( method == 'Saved Blocks' ) {
       if( head( as.Date( rownames( lev ) ), 1 ) <= as.Date( '2004-3-31' ) ) {
         sim.levels = simulate.from.blocks( levels = lev, initial.state = state, nday.sim = nday.sim )[ nday.sim, ];
         sim.rtns = -1 + sim.levels;
       } else {
         sim.rtns = simulate.returns( lev, ndays = nday.sim, block.size = block.size, num.sims = num.sims, method = 'Markov' );
       };
    } else {
      sim.rtns = simulate.returns( lev, ndays = nday.sim, block.size = block.size, num.sims = num.sims, method = method );
    };

    dim( sim.rtns ) = c( num.sims, 1 );
    MV = cbind( MV, mktvals[i] * sim.rtns );
  };
  
  if( identical( VaR.pct, c() ) ) {
    return( MV );
  } else {
    total.MV = rowSums( MV );
    return( quantile( total.MV, 1 - VaR.pct ) );
  };
};

##########################################################################################################
# Routine: piecewise.smooth
#
# Separate the data into regions, and fit the function separately in each region, and smoothly rejoin the 
# fitted functions
##########################################################################################################

piecewise.smooth <- function( 
  x, 
  y, 
  degree   = 3, 
  nBuckets = 3, 
  lower.bound = function(x) -Inf, 
  upper.bound = function(x)  Inf, 
  right.asymp = NA,
  left.asymp  = NA, 
  start.vals  = list( right = list(), left = list() ), 
  range       = c( 0, 1.2 )
) {
  numBuckets = nBuckets + as.numeric( !identical( NA, right.asymp ) ) + as.numeric( !identical( NA, left.asymp ) ); 

  bins = seq( from = 1, to = length( x ), by = ( length(x) - 1 ) / numBuckets );

  prediction.points = seq( from = range[1] * min( x ), to = range[2] * max( x ), len = 10 * length( x ) );
  
  fitted.values = c();
  overlapping.X = c();
  for( i in 1:numBuckets ) {
    startInd = max( bins[i]   - 2, 1 );
    endInd   = min( bins[i+1] + 2, length( x ) );

    X = x[ startInd:endInd ];
    Y = y[ startInd:endInd ];

    res = lm( Y ~ poly( X, degree ) );
    if( i == 1 ) {
      xi = prediction.points[ prediction.points <= x[endInd] ];
      res = if( !identical( NA, left.asymp ) ) nls( left.asymp, data.frame( X, Y ), start.vals$left );
    } else if ( i == numBuckets ) {
      xi = prediction.points[ prediction.points >= x[startInd] ];
      res = if( !identical( NA, right.asymp ) ) nls( right.asymp, data.frame( X, Y ), start.vals$right );
    } else {
      xi = prediction.points[ x[ startInd ] <= prediction.points & prediction.points < x[ endInd ] ];
    };

    yi = as.numeric( predict( res, data.frame( X = xi ) ) );

    # Impose upper and lower bounds
    yi = pmin( yi, upper.bound( xi ) );
    yi = pmax( yi, lower.bound( xi ) );
  
    matches = sum( !is.na( match( overlapping.X, xi ) ) );
    if( length( matches ) > 0 ) {
      inds  = ( length( fitted.values ) - matches + 1 ):length( fitted.values );
      adj.y = ( ( matches:1 ) * fitted.values[ inds ] + (1:matches)*yi[ 1:matches ] ) / ( matches + 1 );
      fitted.values[ inds ] = adj.y;
    };

    fitted.values = c( fitted.values, yi[ -(1:matches ) ] );
    overlapping.X = c( overlapping.X, xi[ -(1:matches ) ] );
  };    
  
  agg = data.frame( x = overlapping.X, y = fitted.values ); 
  return( agg ); 
};

##########################################################################################################
# Routine: bucketize
#
# Given a dependent and independent series, return the means and stddevs
##########################################################################################################

bucketize <- function( 
  dependent, 
  independent, 
  bucket.size = 50
) {
  indep <- independent[ order( dependent ) ];
  dep   <- sort( dependent );

  breaks = round( seq( from = 1, to = length( indep ), length.out = round( length( indep ) / bucket.size ) ) );
  means = c();
  std   = c();
  x     = c();
  for( i in 2:length( breaks ) ) {
    x[i-1] = mean( indep[ breaks[ (i-1):i ] ] );
    means[i-1] = mean( dep[ breaks[ (i-1):i ] ] );
    std[i-1]   = sd( dep[ breaks[ (i-1):i ] ] );
  };

  output = list( X = x, Mean = means, Std = std );
  return( output );
};

##########################################################################################################
# Routine: Test Prediction
#
# Given a dependent and independent variable, test effectiveness of prediction at different lags
##########################################################################################################

test.predictor <- function( y, x, max.lag = 50 ) {
  Corr = numeric(max.lag);
  PV = numeric(max.lag);
  T = numeric(max.lag);
  for( i in 1:max.lag ) {
    rtns = ri( y, i );
    ts = nanmerge( rtns, lag( x, i ) );
    Corr[i] = corr( ts )
    test.results = cor.test( ts[,1], ts[,2], method = 'pearson' );
    PV[i] = test.results$p.value;
    T[i] = test.results$statistic; 
  }; 

  return( list( Correlation = Corr, PValue = PV, TStat = T ) );
};




