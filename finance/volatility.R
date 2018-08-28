
###########################################################################################
# Routine: compare.vol.measures
###########################################################################################

compare.vol.measures <- function( 
  N, 
  num.sims = 1e4, 
  dist     = 'normal', 
  params   = list( mean = 0, std = .01 )
) {
  std     = rep( NaN, num.sims ); 
  c2c     = rep( NaN, num.sims ); 

  for( i in 1:num.sims ) {
    if( dist == 'normal' ) {
      rands = rnorm( N, params$mean, params$std );
    } else {
      stop( paste( 'Unknown distribution type: ', dist ) );
    };

    std[i]     = sd( rands );
    c2c[i]     = tail( vol.close.to.close( rands, N = N ), 1 );
  };

  std.unb = std / std.norm.bias( N );
  c2c.unb = c2c / std.norm.bias( N );

  output = list( Std = std, Std.Unb = std.unb, Close2Close = c2c / sqrt( 252 ), Close2Close.Unb = c2c.unb / sqrt(252) );
  return( output );
};

###########################################################################################
# Routine: vol.close.to.close
#
# Assuming the process is normal, this is the correction to the bias in the standard
# deviation estimator (Cochran's theorem), which comes from the chi distribution. 
###########################################################################################

std.norm.bias <- function( N ) {
  bias = sqrt( 2 / (N-1) ) * gamma( N / 2 ) / gamma( (N-1) / 2 );

  return( bias );
};

###########################################################################################
# Routine: vol.close.to.close
#
# Calculate volatility of returns without demeaming the data
###########################################################################################

vol.close.to.close <- function( 
  close.prices, 
  rtn.method   = 'compound', 
  N 	       = NA
) {
  # Check to see if the input was daily returns or levels
  if( 2 == sum( c( 1, -1 ) %in% sign( as.numeric( close.prices ) ) ) ) {
    rtns = close.prices;
  } else {
    rtns     = returns( close.prices, method = rtn.method );
  };

  num.rtns = as.numeric( rtns[ !is.na( rtns ) ] );

  if( is.na( N ) ) {
    variance = cumsum( num.rtns * num.rtns ) / 0:( length( num.rtns ) - 1 ); 
  } else {
    variance = rep( NaN, length( num.rtns ) - N + 1 ); 
  
    variance[1] = sum( num.rtns[1:N] * num.rtns[1:N], na.rm = TRUE ) / ( N-1 );

    if( length( variance ) > 1 ) {
      for( i in 2:length( variance ) ) {
        variance[i] = variance[i-1] - num.rtns[i-1] * num.rtns[i-1] / (N-1) + num.rtns[i+N-1] * num.rtns[i+N-1] / (N-1);
      }; 
    };
  };

  vol = sqrt( 252 * variance );
  vol = vol[ !is.na( variance ) & is.finite( variance ) ];

  if( class( close.prices ) == 'timeSeries' ) {
    vol = timeSeries( vol, tail( rownames( rtns ), length( variance ) ) );
  } else {};

  return( vol );
};

###########################################################################################
# Routine: calc.vol
#
# Calc volatility using various methods and notaion detailed in Yang & Zhang
###########################################################################################

calc.vol <- function( 
  data, 
  method = 'Standard', 
  N = NA
) {
  variance = calc.variance( data = data, method = method, N = N );
  vol = sqrt( variance );
  return( vol )
};

###########################################################################################
# Routine: vol.yz
#
# Calculate the volatility, using the methodology of Yang-Zhang
###########################################################################################

vol.yz <- function( 
  data, 
  N = NA
) {
  vol = calc.vol( data, method = 'Yang-Zhang', N = N );
  return( vol );
};

###########################################################################################
# Routine: vol.gk
#
# Calculate the volatility, using the methodology of Garman-Klass
###########################################################################################

vol.gk <- function(
  data,
  N = NA
) {
  vol = calc.vol( data, method = 'Garman-Klass', N = N );
  return( vol );
};


###########################################################################################
# Routine: calc.variance
#
# Calc variance using various methods and notaion detailed in Yang & Zhang
###########################################################################################

calc.variance <- function( 
  data, 
  method = 'Standard', 
  N = NA
) {
  if( is.na( N ) ) {
    N = nrow( data ) - 1;
  };

  C_0 = data$Close[-nrow( data ) ];
  C_1 = data$Close[-1];
  O_1 = data$Open[-1];
  H_1 = data$High[-1];
  L_1 = data$Low[-1];
  
  o = log( O_1 ) - log( C_0 );
  u = log( H_1 ) - log( O_1 );
  d = log( L_1 ) - log( O_1 );
  c = log( C_1 ) - log( O_1 );

  V_O  = o * o;
  V_C  = c * c;
  V_P  = 1/( 4 * log(2) ) * ( u - d )^2;
  V_RS = u * ( u - c ) + d * ( d - c );

  if( method == 'Standard' ) {
    variance = N/(N-1) * { SMA( (o+c)^2, n = N ) - SMA( o+c, n = N )^2 };
  } else if( method == 'Parkinson' ) { 
    variance = SMA( V_P, n = N );
  } else if( method == 'Rogers-Satchell' ) {
    variance = SMA( V_RS, n = N );
  } else if( method == 'Garman-Klass' ) {
    variance = SMA( V_O - 0.383 * V_C + 1.364 * V_P + 0.019 * V_RS, n = N );
  } else if( method == 'Yang-Zhang' ) {
    V_Obar = SMA( o^2, n = N ) - SMA( o, n = N )^2
    V_Cbar = SMA( c^2, n = N ) - SMA( c, n = N )^2
 
    k = 0.34 / ( 1.34 + (N+1)/(N-1) );

    variance = N/(N-1) * { V_Obar + k * V_Cbar + ( 1 - k ) * SMA( V_RS, n = N ) };
  } else {
    stop( paste( 'Unsupported method in calc.variance:', method ) );
  };

  dates = as.Date( data$Date[-1] );
  varianceTS = timeSeries( variance, dates, units = data$Symbol[1] );

  return( varianceTS );
};

###########################################################################################
# Routine: construct.rolling.sum
#
# Perform a sum
###########################################################################################

construct.rolling.sum <- function( 
  series, 
  N = NA
) {
  if( is.na( N ) ) {
    output = cumsum( series );
  } else {
    output = rep( NaN, length( series ) );
    output[N] = sum( series[1:N] );
    for( i in (N+1):length( series ) ) {
      output[i] = output[i-1] + series[i] - series[ i - N ];
    };
  };
 
  return( output );
};



