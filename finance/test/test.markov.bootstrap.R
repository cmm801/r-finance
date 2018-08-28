
test.sim.rtns <- function( 
  symbol          = '^GSPC',
  test.set.size   = .2,
  test.block	  = TRUE, 
  nBuckets        = 10,
  nday.sim        = 20,
  block.size      = 2, 
  method          = 'Markov', 
  use.overlapping = TRUE
) {
  VXO    = get.time.series( '^VXO', start.date = today() - years(30), as.Date( '2012-05-04' ) );
  levels = get.ts.or.proxy( symbol, start.date = today() - years(30), as.Date( '2012-05-04' ) );
  levels = levels[ !is.na( match( rownames( levels ), rownames( VXO ) ) ), ]

  rtns = calc.returns( levels, nday.rtn = nday.sim );
  data = data.frame( levels[ 1:length( rtns ), ] / levels[1], Returns = as.numeric( rtns ) );
 
  if( use.overlapping == FALSE )
    data = data[ seq( from = 1, to = nrow( data ), by = nday.sim ), ];

  if( test.block == TRUE ) {
    block.start = floor( runif(1) * ( 1 - test.set.size ) * nrow( data ) )
    training.locs = block.start:floor( block.start * ( 1 + test.set.size ) );
  } else {
    training.locs = sample( 1:length( levels ), length( levels ) * ( 1 - test.set.size ), replace = FALSE );
  };

  training.data   = data[  training.locs, ];
  validation.data = data[ -training.locs, ];

  if( method == 'Markov' ) {
    initial.states = find.vix.state( nBuckets = 10, asOfDate = as.Date( rownames( validation.data ) ), 
							start.date = start.date, end.date = end.date );

    uniq.states = sort( unique( states ) );

    all.obs  = lapply( rep( NaN, nBuckets ), function(x) x )
    all.sims = lapply( rep( NaN, nBuckets ), function(x) x )
    for( state in uniq.states ) {
      print( state );
      ts = timeSeries( training.data[, 1 ], rownames( training.data ) );
      all.sims[[ state ]] = sort( markov.vix.bootstrap( training.data[, 1], nday.sim = nday.sim, block.size = block.size, 
					  	num.buckets = nBuckets, initial.state = state, start.date = start.date, end.date = end.date ) );
      locs = which( states == state );
      locs = locs[ locs <= length( levels.tail ) ];
      fut.rtns = levels.tail[ locs + nday.sim ] / levels.tail[ locs ];
      all.obs[[ state ]] = approx( all.sims[[ state ]], 1:length( all.sims[[ state ]] ), fut.rtns )$y / length( all.sims[[state]] );
    }
  } else if( method == 'Normal' ) {
    all.sims = rnorm( nrow( levels.tail ), mean( rtns[1:split.ind], na.rm = TRUE ), sd = sd( rtns[1:split.ind], na.rm = TRUE ) );
  } else {
    stop( paste( 'Unsupported method: ', method ) );
  };

  } else if( method == 'Normal' ) {
    fut.rtns = rtns[ (split.ind+1):length(rtns)]
    all.obs = list( approx( all.sims[[1]], 1:length( all.sims[[1]] ), fut.rtns )$y / length( all.sims[[1]] ) );
  }

  return( all.obs )
};

calc.sim.stderr <- function( dist ) {
  dist = dist[ !is.na( dist ) ];
  R = ( sort( dist ) - ( 1:length( dist ) ) / length( dist ) );
  rsquare = sum( R * R ) / length( dist );
  return( rsquare );
};

calc.unif.stderr <- function( N = 1e3, K = 1e3 ) {
  nums = seq( from = 10, to = N, by = 10 );

  rsquare = rep( NA, length( nums ) );
  stderr  = rep( NA, length( nums ) );
  
  for( i in 1:length( nums ) ) {
    tmp = rep( NaN, K );
    for( j in 1:K )
      tmp[j] = calc.sim.stderr( runif( nums[i] ) );

    rsquare[i] = mean( tmp );
    stderr[i]  = sd( tmp );
  };

  output = data.frame( Trials = nums, RSquare = rsquare, StdErr = stderr );
  return( output )
};

test.markov <- function(
  method   = 'Markov', 
  nday.sim = 20
) {
  if( method == 'Markov' ) {
    block.sizes = c( 1, 2, 3, 5, 10, 20, 50, 100 );
  } else if( method == 'Normal' ) {
    block.sizes = 1 
  } else {
    stop( paste( 'Unsupported method: ', method ) );
  }
 
  results = lapply( rep( NaN, length( block.sizes ) ), function(x) x )
  for( i in 1:length( block.sizes ) ) {
    print( sprintf( 'Block size %s', block.sizes[i] ) );
    res = unlist( test.sim.rtns( block.size = block.sizes[i], nday.sim = nday.sim, method = method ) );
    results[[i]] = res[ !is.na( res ) ];
  };

  return( results );
};

