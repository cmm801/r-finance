

###########################################################################################
# Routine: get.futures.term.structure
###########################################################################################

get.futures.term.structure <- function( asOfDate = last.biz.date() ) {
  fut = get.futures.data();
  tmp = fut[ fut$Trade_Date == asOfDate & fut$Open_Interest > 0, ]
  ts  = timeSeries( tmp$Close, tmp$Expiration_Date );
  return( ts );
};

###########################################################################################
# Routine: get.futures.data
#
###########################################################################################

get.futures.data <- function() {
  colClasses = c( Trade_Date = 'Date', Futures = 'character', Open = 'numeric', High = 'numeric', Low = 'numeric', Close = 'numeric', 
			Settle = 'numeric', Change = 'numeric', Total_Volume = 'numeric', EFP = 'numeric', Open_Interest = 'numeric' );

  data = read.csv( VIX_CSV, stringsAsFactors=FALSE, sep = "\t", header = TRUE, colClasses = colClasses );

  get.yearMonths <- function( col ) {
    monthAndYear = unlist( strsplit( gsub(".*\\(([^()]*)\\).*", "\\1", col ), ' ' ) );

    month = match( monthAndYear[1], month.abb );
    year  = 2000 + as.numeric( monthAndYear[2] );

    yearMonth = year*100 + month;
    return( yearMonth );
  };

  yearMonths = unlist( lapply( unique( data$Futures ), get.yearMonths ) );
  all.expiries = get.expiries( 'VIX' );
  
  locs = match( yearMonths, unlist( lapply( all.expiries, function(x) year(x)*100 + month(x) ) ) );

  uniq.Expiration.Date <- as.Date( all.expiries[ locs ] );
  Expiration.Dates <- uniq.Expiration.Date[ match( data$Futures, unique( data$Futures ) ) ];
  
  data <- cbind( data, data.frame( Expiration_Date = Expiration.Dates ) );
  Days.to.Expiry <- as.numeric( data$Expiration_Date ) - as.numeric( as.Date( data$Trade_Date ) );
  data <- cbind( data, data.frame(Days_to_Expiry = Days.to.Expiry ) );
  data <- data[ data$Settle > 0, ];

  return( data );
};

###########################################################################################
# Routine: get.otr.futures
#
# Get On the Run (otr) futures
# If 'roll.before.expiry' is TRUE, then the on-the-run first month futures contract on the
# expiration date will be the contract that expires one month hence.  
###########################################################################################

get.otr.futures <- function( 
  data.type 	     = 'Settle', 
  rm.zeros 	     = TRUE, 
  roll.before.expiry = TRUE, 
  as.returns         = FALSE
) {
  # Get all Futures Data
  data = get.futures.data();

  # Order data by Expiration and Trade Dates
  data = data[  order( data$Expiration_Date, data$Trade_Date ), ]

  # Remove data where Trade and Expiry dates are the same, if 'roll.before.expiry' is TRUE
  if( roll.before.expiry == TRUE ) 
    data = data[ data$Trade_Date != data$Expiration_Date, ];

  if( as.returns == TRUE ) {
    all.expiries = sort( unique( data$Expiration_Date ) );

    data[ data[[ data.type ]] == 0, data.type ] = NaN
    data[ , data.type ] = na.locf( data[, data.type ], na.rm = FALSE );
 
    for( i in 1:length( all.expiries ) ) {
       rtns = na.locf( returns( data[ data$Expiration_Date == all.expiries[i], data.type ], method = 'discrete' ), na.rm = FALSE );
       
       data[ data$Expiration_Date == all.expiries[i], data.type ] = rtns;
    };
  }

  dates = sort( unique( as.Date( data$Trade_Date, '%M/%d/%Y' ) ) );

  # Determine how many expiries exist between a given Trade_Date and Expiration_Date  
  VIX.expiries = get.expiries( 'VIX', start.date = as.Date( '2004-01-01' ) ); 
  date.position = unlist( lapply( data$Trade_Date, function(x) max( which( as.Date( x ) >= as.Date( VIX.expiries ) ) ) ) )
  exp.months = match( data$Expiration_Date, VIX.expiries ) - date.position;

  # Form a data frame with columns corresponding with the number of months to expiry
  on.the.run.data <- matrix( NaN, length( dates ), ceiling( max( data$Days_to_Expiry ) / 30 ) ); 
  for( m in sort( unique( exp.months ) ) ) {
    # Create vectors of character keys, so that we can use the 'match' fn to match both Trade Date and Months until Expiration
    otr.keys  = sprintf( '%s %d', dates, m );
    data.keys = sprintf( '%s %d', data$Trade_Date, exp.months );
    on.the.run.data[, m ] = data[ match( otr.keys, data.keys), data.type ]
  };

  if( rm.zeros == TRUE && as.returns == FALSE )
    on.the.run.data[ abs( on.the.run.data ) < 1e-10 ] = NaN;

  otr = timeSeries( on.the.run.data, dates );
  return( otr );
};

###########################################################################################
# Routine: roll.futures
#
# Roll first into second month futures. 
###########################################################################################

roll.futures <- function( 
  data.type = 'Settle'  
) {
  rtns  = get.otr.futures( data.type = data.type, as.returns = TRUE );
  rtns[ is.na( rtns ) ] = 0;

  dates = as.Date( sort( rownames( rtns ) ) ); 
  all.dates = c( min( dates ) - 30, dates, max( dates ) + 30 );

  holidays <- get.holidayNYSE( 2004, year( today() ) );

  first.day.of.month = unlist( lapply( all.dates, function( x ) as.Date( sprintf('%d/%d/01', year( x ), month(x) ) ) ) );
  all.expiries = get.expiries( 'VIX' );
 
  weightsTS = construct.roll.weights( all.expiries, holidays ); 
  weights   = weightsTS[ !is.na( match( as.Date( rownames( weightsTS ) ), dates ) ), ];

  fut.rtns = matrix( weights[ 1:( length( dates ) - 1 ), 1:2 ], ncol = 2 ) * rtns[ 2:length( dates ), 1:2 ];
  cash.ts  = get.time.series.cash( "USDCash", start.date = dates[1], end.date = max( dates ) );
  TBT      = ri( cash.ts[ match( c( dates[1], as.Date( rownames( fut.rtns ) ) ), as.Date( rownames( cash.ts ) ) ), ] );

  TS = cumulated( TBT + fut.rtns[, 1 ] + fut.rtns[, 2 ], method = 'discrete' );
  return( TS );
};

###########################################################################################
# Routine: construct.roll.weights 
#
# Helper function to create the weights used for rolling 1st and 2nd month futures strategies
###########################################################################################

construct.roll.weights <- function( 
  all.expiries, 
  holidays = get.holidayNYSE( year( all.expiries[1] ), year( tail( all.expiries, 1 ) ) ) 
)
{
  biz.dates = get.all.biz.days( min( all.expiries ), max( all.expiries ) );

  weights = matrix( NaN, ncol = 2, nrow = length( biz.dates ) + 1 );
  counter = 1;
  for( i in 2:length( all.expiries ) ) {
     dt = length( biz.dates[ all.expiries[i-1] <= biz.dates & biz.dates < all.expiries[i] ] );
     if( dt != 0 ) {
       dr = seq( from = dt, to = 1, by = -1 );
       
       weights[ counter:( counter + dt - 1 ), ] = cbind( dr / dt, 1 - dr / dt );
     };

     counter = counter + dt;
  };

  weightsTS = timeSeries( weights[ 2:nrow( weights ), ], as.Date( biz.dates ) );
  return( weightsTS );
};

###########################################################################################
# Routine: get.constant.maturity.futures
#
# Interpolate futures time series to create constant maturity futures time series of 30, 60, 90,... days
###########################################################################################

get.constant.maturity.futures <- function( 
  data.type = 'Settle', 
  rm.zeros = TRUE, 
  tenors = seq( from = 0, to = 360, by = 30 )
) {
  raw.data = get.futures.data();
  vix = get.time.series( '^VIX', start.date = as.Date( min( raw.data$Trade_Date ) ), end.date = as.Date( max( raw.data$Trade_Date ) ) );
  vix.df = data.frame( Trade_Date = as.Date( rownames( vix ) ), Values = as.numeric( vix ), Days_to_Expiry = 0 );
  colnames( vix.df )[2] = data.type;
  
  # Remove VIX data on Settlement dates, when there is Futures data with Days_to_Expiry = 0
  expiry.dates = raw.data[ raw.data$Days_to_Expiry == 0, 'Trade_Date'  ];
  vix.df = vix.df[ !( vix.df$Trade_Date %in% expiry.dates ), ];

  data = rbind( raw.data[, colnames( vix.df ) ], vix.df );

  const.mat.ts = convert.to.constant.maturity( data, rm.zeros = TRUE, tenors = tenors );
  return( const.mat.ts );
};

###########################################################################################
# Routine: convert.to.constant.maturity
###########################################################################################

convert.to.constant.maturity <- function(
  data,
  rm.zeros = TRUE,
  tenors = seq( from = 0, to = 360, by = 30 ), 
  data.type = colnames( data )[2]
) {
  if( rm.zeros == TRUE ) {
    data = data[ abs( data[, data.type ] ) > 1e-10, ];
  };

  dates = sort( unique( as.Date( data$Trade_Date ) ) );
 
  constant.maturity.data <- matrix( NaN, length( dates ), length( tenors ) );
  for( i in 1:length( dates ) ) {
    rows = data[ data$Trade_Date == dates[i], ];

    splineVals = spline( rows$Days_to_Expiry, rows[[ data.type ]], n = 10 * length( rows$Days_to_Expiry ) );
    interpolated.values = approx( splineVals$x, splineVals$y, tenors );
    constant.maturity.data[i,] = as.numeric( interpolated.values[[2]] );
  };

  constant.maturity.data[ is.na( constant.maturity.data ) ] = NaN;

  const.mat.ts = timeSeries( constant.maturity.data, dates );
  colnames( const.mat.ts ) = paste( 'CM', tenors, sep = '_' ); 

  return( const.mat.ts );
};

###########################################################################################
# Routine: convert.from.constant.maturity
###########################################################################################

convert.from.constant.maturity <- function( 
  const.mat.ts, 
  tenors = seq( from = 0, to = 360, by = 30 ), 
  include.spot = FALSE
) {
  VIX.expiries = calc.VIX.expiries( start.date = first.date( const.mat.ts ), end.date = today() + days( max(tenors) ) );
  dates = as.Date( rownames( const.mat.ts ) );

  data = data.frame();
  for( i in 1:nrow( const.mat.ts ) ) {
    date = dates[i];
    tmp.exp = VIX.expiries[ date < VIX.expiries & as.numeric( VIX.expiries ) - as.numeric( date ) < max( tenors ) ];

    if( include.spot ) {
      days.to.expiry = c( 0, as.numeric( tmp.exp ) - as.numeric( date ) );
    } else {
      days.to.expiry = as.numeric( tmp.exp ) - as.numeric( date );
    };

    splineVals = spline( tenors, as.numeric( const.mat.ts[i,] ), n = 10 * length( tenors ) );
    fut.vals = approx( splineVals$x, splineVals$y, days.to.expiry );

    tmp.df = data.frame( Trade_Date = date, Values = fut.vals$y, Days_to_Expiry = days.to.expiry );
    if( is.empty( data ) ) { 
      data = tmp.df;
    } else {
      data = rbind( data, tmp.df );
    };
  };

  data = data[ !is.na( data$Values ), ]

  return( data );
};

