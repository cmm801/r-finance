
###########################################################################################
# Routine: get.UST.constant.maturity
#
# Get Constant Maturity UST bond yields
###########################################################################################

get.UST.constant.maturity <- function(
  start.date = today() %m-% years(50),
  end.date   = last.biz.date(), 
  freq = 'D'
) {
  if( freq == 'D' ) {
    Symbols  <- c( 'DGS1MO', 'DGS3MO', 'DGS6MO', 'DGS1', 'DGS2', 'DGS3', 'DGS5', 'DGS7', 'DGS10', 'DGS20', 'DGS30' );
    maturity <- c( 1/12, 1/4, 1/2, 1, 2, 3, 5, 7, 10, 20, 30 );
  } else {
    Symbols = c( 'GS3M', 'GS1', 'GS2', 'GS3', 'GS5', 'GS7', 'GS10', 'GS20', 'GS30' );
    maturity = c( 1/4, 1, 2, 3, 5, 7, 10, 20, 30 );
  };

  data = get.fed.data( Symbols, start.date = start.date, end.date = end.date );
  data = data[ rowSums( data, na.rm = T ) != 0, ]; 
  return( list( Maturity = maturity, Yield = data ) );
};

###########################################################################################
# Routine: get.UST.term.structure
#
# Get term structure of UST bond yields
###########################################################################################

get.UST.term.structure <- function( dates ) {
  output = list();  
  for( i in 1:length( dates ) ) {
     tmp = get.UST.constant.maturity( start.date = dates[i] - 1, end.date = dates[i] + 1 );
     if( dates[[i]] %in% rownames( tmp$Yield ) ) {
       yields = as.numeric( tmp$Yield[ dates[i], ] );
     } else if( length( tmp$Yield ) > 0 ) {
       yields = as.numeric( tmp$Yield[ 1, ] );
     } else {
       yields = NA * tmp$Maturity
     };
      
     output[[i]] = list( x = tmp$Maturity, y = yields );
  } 
  
  return( output );
};

###########################################################################################
# Routine: calc.bond.index
#
# Create a proxy bond index given a certain maturity.
###########################################################################################

calc.bond.index <- function( maturity, freq = 'D' ) {
  yields = get.bond.yield( maturity, 'D' );

  if( freq == 'M' ) {
    yields = daily2monthly( yields );
  };

  dates = as.Date( rownames( yields ) );
  coupon = lag( yields );

  old.clean.prices = calc.bond.price( maturity = maturity, ytm = lag(yields), coupon = coupon );
  new.clean.prices = calc.bond.price( maturity = maturity, ytm = yields, coupon = coupon);

  if( freq == 'D' ) {
    new.dirty.prices = new.clean.prices + 100 * coupon * c( NA, diff( as.numeric( dates ) ) / 360 );
  } else if( freq == 'M' ) {
    new.dirty.prices = new.clean.prices + 100 * coupon * rep( 1/12, length( dates ) );
  };

  rtns = -1 + new.dirty.prices / old.clean.prices;
  UST = ind.ts( prod.ts( rtns ) );
  return( UST );
};


###########################################################################################
# Routine: get.bond.yield
###########################################################################################

get.bond.yield <- function( maturity, freq = 'D' ) {
  if( freq == 'D' ) {
    symbol = sprintf( 'DGS%d', maturity );
  } else if( freq == 'M' ) {
    symbol = sprintf( 'GS%d', maturity );
  } else {
    stop( paste( 'Unsupported frequency:', freq ) );
  };

  yields = get.fed.data( symbol ) / 100;
  yields[ yields == 0 ] = NA
  yields = yields[ !is.na( yields ), ]

  return( yields );
};

###########################################################################################
# Routine: calc.bond.price
#
# Given maturity and Yield-to-Maturity, price the bond assuming regular coupons.
###########################################################################################

calc.bond.price <- function( 
  maturity, 
  ytm, 
  coupon = ytm,
  coupon.freq = 2 
) {
  coupon.PV = numeric( length( ytm ) ) * NaN;
  for( i in 1:length( coupon.PV ) ) {
    coupon.PV[i] = sum( coupon[i]/coupon.freq * 100/( 1 + ytm[i] )^( ( 1:(maturity*coupon.freq ) ) / coupon.freq ) );
  };

  price = coupon.PV + 100/( 1 + ytm )^(maturity);
  return( price );
};

###########################################################################################
# Routine: calc.bond.duration
#
# Given maturity and Yield-to-Maturity, calculate the duration of the bond.
###########################################################################################

calc.bond.duration <- function( maturity, ytm, coupon.freq = 2 ) {
  # This value of epsilon is chosen to give the usual value of the duration.  If it is made smaller, FP error will give wrong duration
  eps = 1e-10;

  duration = -( -1 + calc.bond.price( maturity = maturity, ytm = ytm + eps, coupon = ytm ) / 
		     calc.bond.price( maturity = maturity, ytm = ytm ) ) / eps;

  return( duration );
};


###########################################################################################
# Routine: proxy.bond.index
# 
# Given a duration, proxy a bond index returns
###########################################################################################

proxy.bond.index <- function( duration = 5, freq = 'D' ) {

  maturity = c( 1, 2, 3, 5, 7, 10, 20, 30 );
  symbol.names = c( 'DGS1', 'DGS2', 'DGS3', 'DGS5', 'DGS7', 'DGS10', 'DGS20', 'DGS30' );

  UST = get.fed.data( symbol.names );
  UST[ UST == 0 ] = NA;

  indicator.names = 
  test.indicators = matrix( UST[ is.na( UST[ , response.names ] ) & !is.na( rowSums( UST[ , indicator.names ] ) ), indicator.names ],
                                                                                ncol = length( indicator.names ) );

  training.indicators = matrix( UST[ !is.na( rowSums( UST ) ), indicator.names ], ncol = length( indicator.names ) );
  training.responses  = matrix( UST[ !is.na( rowSums( UST ) ), response.names ] )

};

###########################################################################################
# Routine: proxy.rand.forest
# 
# Reconstruct missing bond data using random forest
###########################################################################################

proxy.rand.forest <- function( indicators, responses, test.indicators ) {
  X = matrix( indicators, ncol = ncol( indicators ) );
  Y = matrix( responses );

  X0 = matrix( test.indicators, ncol = ncol( test.indicators ) );

  rf = new( 'PredictRandomForest', indicators = indicators, responses = responses );
  rf = train( rf );

  predictedValues = predict( rf, X0 );  
  test.dates = row.names( test.indicators );
  fitted.ts = timeSeries( predictedValues, test.dates, units = colnames( responses ) );
  return( fitted.ts );
};


