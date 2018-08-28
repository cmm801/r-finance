

vxx.test.data <- function() {
  vix = get.time.series( '^VXO', start.date = today() - years(30) );
  vxx = get.ts.or.proxy( 'VXX',  start.date = today() - years(30) );

  vix.rtns = ri( vix );
  names( vix.rtns ) = 'VXO_rtns';
  vxx.rtns = ri( vxx );

  lagvix.rtns = lag( vix.rtns, 1 );
  names( lagvix.rtns ) = 'VX0_rtns_0';

  all.ts = merge.ts( list( vix.rtns, lagvix.rtns, vix, ri( vxx ) ) );
  all.dates = as.Date( row.names( all.ts ) );

  training.dates = all.dates[ !is.na( rowSums( all.ts ) ) ];
  training.indicators = matrix( all.ts[ training.dates, 1:3 ], ncol = 3 );
  training.responses = matrix( all.ts[ training.dates, 4 ] );
  
  test.dates = all.dates[ all.dates < as.Date( '2004-03-31' ) & !is.na( rowSums( all.ts[, 1:3 ] ) ) ];
  test.indicators = matrix( all.ts[ test.dates, 1:3 ], ncol = 3 );

  data = list( X = training.indicators, Y = training.responses, X0 = test.indicators, 
				training.dates = training.dates, test.dates = test.dates );
  return( data );
};

vxx.predict.rf <- function() {
  data = vxx.test.data();
  rf = new( 'PredictRandomForest', indicators = data$X, responses = data$Y );
  rf = train( rf );
  predictedValues = predict( rf, rbind( data$X0, data$X ) );
  vxx = timeSeries( predictedValues, c( data$test.dates, data$training.dates ), 'VXX' );
  return( vxx );
};

ust.test.data <- function( ) {
  indicator.names = c( 'DGS5', 'DGS10', 'DGS20' );
  response.names = 'DGS30';
  UST = get.fed.data( c( indicator.names, response.names ), start.date = as.Date( '1993-10-01' ) );
  UST[ UST == 0 ] = NA;

  test.indicators = matrix( UST[ is.na( UST[ , response.names ] ) & !is.na( rowSums( UST[ , indicator.names ] ) ), indicator.names ], 
										ncol = length( indicator.names ) );

  training.indicators = matrix( UST[ !is.na( rowSums( UST ) ), indicator.names ], ncol = length( indicator.names ) );
  training.responses  = matrix( UST[ !is.na( rowSums( UST ) ), response.names ] )
  
  KNN = new( 'PredictKNN', indicators = training.indicators, responses = training.responses );

};

fed.test.data <- function( ind = 'INDPRO', level = TRUE ) {
  fred.data = get.fed.data( ind );
  ff.data = get.ff.data()[ , 'Mkt_RF' ];

  if( !level ) {
    fred.data = ri( fred.data )
  };

  ts = merge( fred.data, ff.data );
  clean.ts = ts[ !is.na(rowSums(ts)), ];



  date.diffs = diff( as.numeric( as.Date( row.names( clean.ts ) ) ) );
  if( max( date.diffs ) > 31 || min( date.diffs ) < 28 ) {
    stop( 'Missing dates' )
  };

  N = nrow( clean.ts );
  X = matrix( clean.ts[ 1:(N-1), ind ], ncol = length(ind) );
  Y = matrix( clean.ts[ 2:N, 'Mkt_RF' ] );
  dt = list( X = X, Y = Y );
  return( dt );
};


vix.test.data <- function( N = 10, sort.data = FALSE ) {
  ts = get.time.series( c( '^GSPC', '^VXO' ), start.date = today() - years(20) );
  inds = seq( from = 1, to = nrow( ts ), by = N );

  SPX = matrix( ri( ts[inds, 1 ] ) );
  VIX = matrix( ts[inds[-length(inds)], 2 ] );

  if( sort.data ) {
    SPX = matrix( SPX[ order( VIX ) ] );
    VIX = matrix( VIX[ order( VIX ) ] );
  };
 
  return( list( X = VIX, Y = SPX ) ); 
};


smooth.prob.density <- function( Y, weights, sigma = 5 ) {
  smoothed = Y*NaN;
  for( i in 1:length(Y) ) {
    smoothed[i] = weights[i]/2  + sum( dnorm( abs( Y[i] - Y[-i] ) / sigma ) * weights[-i] );
  };

  return( smoothed );
};

data.sine <- function() {
  nObs = 500;
  nCols = 1;
  x = matrix( 1:(nCols*nObs), ncol = nCols );
  y = matrix( sin( x*10/nObs ) ) + .5 * rnorm( nObs );
  return( list( X = x, Y = y ) );
};


##############################################################################################################################
#e Routine: predict.response
#
# Takes two time series (indicators and response), and predicts what the response will be on the prediction.date
##############################################################################################################################

predict.response <- function( 
  indicators, 
  response, 
  prediction.date, 
  method, 
  prediction.horizon = 12, 
  training.frequency = 1, 
  kNN = NA, 
  buffer = NA, 
  weight.method = NA, 
  nTrees = NA, 
  bandwidth = NA, 
  p.regress = NA
) {
  # Lag the reponse variable so that its future values line up with the indicators
  lag.response = lag( response, -prediction.horizon );
  response.name = '_RESPONSE_';
  colnames( lag.response ) = response.name;
 
  # Merge the indicators and response vars to make sure dates line up 
  ts = merge.ts( list( lag.response, indicators ) );

  # Remove any rows with NA's in any variables
  clean.ts = ts[ !is.na( rowSums( ts ) ), ];

  # Remove dates after (and including) the prediction date for Training Data
  pred.loc <- which( rownames( clean.ts ) == prediction.date );
  training.rows = 1:( pred.loc - 1 );
  pred.ts <- clean.ts[ training.rows, ];

  # Separate the response and indicator variables
  Y = as.numeric( pred.ts[ , response.name ] );
  X = matrix( as.numeric( pred.ts[ , colnames( pred.ts ) != response.name ] ), nrow = nrow( pred.ts ), byrow = TRUE );

  # Define the observed response value Y0 and the input data for Test Data  X0
  test.rows = pred.loc:min( nrow( clean.ts ), ( pred.loc + training.frequency - 1 ) );
  Y0 = as.numeric( clean.ts[ test.rows,  1 ] );
  X0 = matrix( as.numeric( clean.ts[ test.rows, -1 ] ), ncol = ncol( clean.ts ) - 1, byrow = TRUE );

  # Call the function specific to a given method
  if( method == 'kNN' ) {
    prediction = predict.kNN( X, Y, X0, kNN = kNN, buffer = buffer, weight.method = weight.method );
  } else if( method == 'RandomForest' ) {
    prediction = predict.RandomForest( X, Y, X0, nTrees = nTrees );
  } else if( method == 'LinearRegression' ) {
    prediction = predict.linear.regression( X, Y, X0 );
  } else if( method == 'Loess' ) {
    prediction = predict.LOESS( X, Y, X0, bandwidth = bandwidth, weight.method = weight.method, p.regress = p.regress );
  } else {
    print( paste( 'Unsupported prediction method: ', method ) );
  };

  output = list( Predicted = prediction, Actual = Y0 ); 
  return( output );
};


##############################################################################################################################
# Routine: predict.LOESS
#
# Predicts a response using the Loess algorithm
##############################################################################################################################

predict.LOESS <- function( 
  X, 
  Y, 
  X0, 
  bandwidth, 
  buffer = 1, 
  weight.method = 'tricube', 
  p.regress = 1
) {
  prediction = c();

  for( r in 1:nrow( X0 ) ) {
    kNN = floor( bandwidth * nrow( X ) );

    row = matrix( X0[r, ], nrow = 1 );

    locs = calc.nearest.neighbors( X, row, kNN = kNN, buffer = buffer );

    ordered.dist = as.numeric( distances[ locs, ] );
    u = ordered.dist / max( ordered.dist );

    weights = calc.kNN.weights( u, weight.method );

    if( p.regress == 0 ) {
      new.pred = sum( weights * Y[ locs ] );
    } else if( p.regress == 1 ) {

      reg = lm( Y[ locs ] ~ X[ locs, ], weights = weights );

      alpha = reg[[1]][ 1];
      beta  = reg[[1]][-1];

      new.pred = alpha + sum( beta * row );
      names( new.pred ) = NULL;
    } else {
      stop( 'This function is only supported for p.regress <= 1' );
    };
    
    prediction <- c( prediction, new.pred );
  };

  return( prediction );
};


##############################################################################################################################
# Routine: predict.kNN
#
# Predicts a response using the k Nearest Neighbors algorithm
##############################################################################################################################

predict.kNN <-  function( 
  X, 
  Y, 
  X0, 
  kNN, 
  buffer = 1, 
  weight.method = 'tricube'
) {
  prediction = c();

  for( r in 1:nrow( X0 ) ) {
    row = matrix( X0[r, ], nrow = 1 );

    locs = calc.nearest.neighbors( X, row, kNN = kNN, buffer = buffer );

    ordered.dist = as.numeric( distances[ locs, ] );
    u = ordered.dist / max( ordered.dist );

    weights = calc.kNN.weights( u, weight.method );

    prediction <- c( prediction, sum( weights * as.numeric( Y[ locs ] ) ) );
  };

  return( prediction );
};

##############################################################################################################################
# Routine: calc.nearest.neighbors
#
# Given a matrix X, find the kNN nearest neighbors of X0.  
# If the option argument 'buffer' is provided, then the function selects nearest-neighbors that are separated by at least
#  'buffer' locations from one another.
##############################################################################################################################

calc.nearest.neighbors <- function( 
  trainingIndicators, 
  X0, 
  kNN
) {
  # Create a time series of the Percentiles of the indicator data
  percentiles = X * NaN;
  P0 = numeric( ncol( X ) );

  for( col in 1:ncol( X ) ) {
    percentiles[, col ] = order( order( X[, col ] ) ) / nrow( X );
  };

  testPercentiles = testIndicators * NaN;
  for( i in 1:ncol( percentiles ) ) {
    
    ordered = order( order( c( X0[ i ], as.numeric( percentiles[, i ] ) ) ) );

    # Calculate the percentiles of the training data
    percentiles[, i ] = 100 * ordered[-1] / max( ordered );

    # Calculate the percentile of the prediction data
    P0[i] = 100 * ordered[1] / max( ordered );
  };

  current_vals = matrix(  P0, nrow = nrow( X ), ncol = ncol( X ), byrow = T )

  differences = percentiles - current_vals;
  distances = timeSeries( sqrt( rowSums( differences * differences ) ) );

  ordered.locs <- order( distances );

  locs <- c( ordered.locs[1] );
  counter <- 2;

  while( length( locs ) < kNN + 1 & counter <= length( ordered.locs ) ) {
    if( length( locs ) == 0 || min( abs( ordered.locs[ counter ] - locs ) )  > buffer ) {
      locs <- c( locs, ordered.locs[ counter ] );
    };

    counter <- counter+1;
  };

  return( locs );
};



##############################################################################################################################
# Routine: calc.kNN.weights
#
# For any of the several methods, calculate the weights to use when forming a prediction
##############################################################################################################################

calc.kNN.weights <- function( x, weight.method ) {
  if( weight.method == 'simple' )  {
    weights = 1 + numeric( length(x) );
  } else if( weight.method == 'tricube' ) {
    weights = ( 1 - x^3 )^3;
  } else if( weight.method == 'bisquare' ) {
    weights = ( 1 - x^2 )^2;
  } else {
    stop( paste( 'Unsupported weight method: ', weight.method ) );
  };

  # Normalize the sum of the weights to 1
  weights = weights / sum( weights );

  return( weights );
};

##############################################################################################################################
# Routine: predict.linear.regression
#
# Predicts a response using Linear Regression
##############################################################################################################################

predict.linear.regression <- function( 
  X, 
  Y, 
  X0
) {
  reg = lm( Y ~ X );
 
  alpha = reg$coefficients[[1]]; 
  beta  = reg$coefficients[-1];

  prediction = alpha + X0 %*% matrix( beta, ncol = 1 ); 
  return( prediction );
};


