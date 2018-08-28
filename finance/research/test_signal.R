
###########################################################################################
# Routine: get.freq.adjustment
#
# 
###########################################################################################

get.freq.adjustment <- function( freq ) {
  if( freq == 'monthly' ) { 
    freq.adj = 12;
  } else if( freq == 'weekly' ) {
    freq.adj = 52;
  } else if( freq == 'daily' ) {
    freq.adj = 255;
  };
  
  return( freq.adj );
}


###########################################################################################
# Routine: lag.test.signal
#
# Lag the signal into the future so that it can be a valid response. 
###########################################################################################

lag.test.signal <- function( dates, signal, prediction.horizon ) {
  first.elems = length( signal ) - prediction.horizon;
  response = timeSeries( head( signal, first.elems ), dates[ -(1:prediction.horizon ) ], 'Response' );
  return( response );
};


###########################################################################################
# Routine: generate.linear.signal
#
# Generate a noisy signal driven by linear noise
###########################################################################################

generate.linear.signal <- function( 
  start.date = today() - years(20), 
  end.date = last.biz.date(), 
  means 	= c( .05 ), 
  ann.stds	= c( .16 ), 
  signal.to.noise = c( 0.5 ), 
  freq 		= 'monthly', 
  prediction.horizon = 12
) {
  dates = get.freq.dates( start.date, end.date, freq = freq );

  if( length( means ) != length( ann.stds ) ) {
    stop( 'The input args Means and Vols must be the same length' );
  };

  freq.adj = get.freq.adjustment( freq );

  mu      = means / freq.adj;
  sigma   = ann.stds / sqrt( freq.adj ); 

  sigma_signal = sigma * signal.to.noise / sqrt( 1 + signal.to.noise^2 );
  sigma_noise  = sigma / sqrt( 1 + signal.to.noise^2 );

  impulse = matrix( rnorm( length( dates ) * length( mu ), mean = mu, sd = sigma_signal ), ncol = length( mu ), byrow = T )   
  noise   = rnorm( nrow( impulse ), mean = 0, sd = sigma_noise );
  signal  = rowSums( impulse ) + noise;
  
  response   = lag.test.signal( dates, signal, prediction.horizon );
  indicators = timeSeries( impulse, dates );

  output = list( response = response, indicators = indicators, Mean = means, Vol = ann.stds, Signal.to.Noise = signal.to.noise );
  return( output );
};


###########################################################################################
# Routine: generate.mean.reverting.signal
#
# Generate a noisy signal driven by linear noise, and with a mean-reverting process
###########################################################################################

generate.mean.reverting.signal <- function( 
  start.date = today() - years(20), 
  end.date = last.biz.date(), 
  means 	= c( .05 ), 
  ann.stds	= c( .16 ), 
  signal.to.noise = c( 0.5 ), 
  freq 		= 'monthly', 
  prediction.horizon = 12, 
  mean.reversion.strength = .2
) {
  dates = get.freq.dates( start.date, end.date, freq = freq );

  if( length( means ) != length( ann.stds ) ) {
    stop( 'The input args Means and Vols must be the same length' );
  };

  freq.adj = get.freq.adjustment( freq );

  mu      = means / freq.adj;
  sigma   = ann.stds / sqrt( freq.adj );

  sigma_signal = sigma * signal.to.noise / sqrt( 1 + signal.to.noise^2 );
  sigma_noise  = sigma / sqrt( 1 + signal.to.noise^2 );

  impulse = matrix( rnorm( length( dates ) * length( mu ), mean = 0, sd = sigma_signal ), ncol = length( mu ), byrow = T )
  noise   = rnorm( nrow( impulse ), mean = 0, sd = sigma_noise );

  X = impulse * NaN;
  Y = impulse * NaN;

  X[1, ] = mu;
  Y[1, ] = mu + noise[1];

  for( i in 2:nrow( impulse ) ) {
    dY = mean.reversion.strength * ( mu - Y[ i-1, ] ) + impulse[ i ];
    dX = mean.reversion.strength * ( mu - X[ i-1, ] ) + impulse[ i ];

    Y[ i, ] = Y[ i-1, ] * exp( dY + noise[ i ] ); 
    X[ i, ] = X[ i-1, ] * exp( dX );
  };
  
  signal  = rowSums( Y );

  response   = lag.test.signal( dates, signal, prediction.horizon );
  indicators = timeSeries( X, dates );

  output = list( response = response, indicators = indicators, Mean = means, Vol = ann.stds, Signal.to.Noise = signal.to.noise, 
							mean.reversion.strength );
  return( output );
};



