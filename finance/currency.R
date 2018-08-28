
###########################################################################################
# Routine: get.fx.spot
###########################################################################################

get.fx.spot <- function(
  from.ccy, 
  to.ccy = NULL,
  asOfDate = last.biz.date()
) {
  sd = asOfDate %m-% months(1);
  ed = asOfDate %m+% months(1);
  fx.rate = get.exchange.rates( from.ccy = from.ccy, to.ccy = to.ccy, start.date = sd, end.date = ed );

  fx.spot = as.numeric( fill.missing( fx.rate, start.date = sd, end.date = ed )[ asOfDate ] );
  return( fx.spot );
};

###########################################################################################
# Routine: get.forward.points
###########################################################################################

get.forward.points <- function( 
  symbol, 
  forward.dates = NULL, 
  pricing.date = NULL,
  BidAsk = 'Mid'
) {
  all.fx.data = read.csv( FORWARD_RATES_RECENT_CSV, sep = '\t', header = TRUE, stringsAsFactors = FALSE );
  current.data = all.fx.data[ all.fx.data$Symbol == symbol, ];

  asOfDates = current.data$AsOfDate;
  if( is.null(pricing.date ) ) {
    fwd.points = current.data[ asOfDates == max( asOfDates ), ];
  } else {
    date.diffs = as.numeric(as.Date(asOfDates)) - as.numeric(as.Date(pricing.date ));
    fwd.points = current.data[ abs(date.diffs) == min( abs( date.diffs ) ), ];
  };

  # Select appropriate data columns, and create Mid quote if necessary
  if( BidAsk == 'Mid' ) {
    fwd.points = data.frame( Forward.Date = fwd.points$Forward.Date, Mid = (fwd.points$Bid + fwd.points$Ask ) / 2 );
  } else {
    fwd.points = fwd.points[, c( 'Forward.Date', BidAsk ) ];
  };

  if( !is.null(forward.dates) ) {
    interp = approx( as.Date( fwd.points$Forward.Date ), fwd.points[,BidAsk ], forward.dates );
    fwd.points = data.frame( interp$x, interp$y );
    colnames( fwd.points ) = c( 'Forward.Date', BidAsk );
  };
    
  return( fwd.points );
};


###########################################################################################
# Routine: get.fx.forward.points.rescale.factor
#
# Get amount by which forward points are rescaled, corresponding to quoting convention decimal places
###########################################################################################

get.fx.forward.points.rescale.factor <- function( symbol ) {
  fx.info = read.csv( EXCHANGE_RATE_INFO_CSV, header = TRUE, stringsAsFactors = FALSE );
  rescale = 10^as.numeric( fx.info[ fx.info$Symbol == symbol, 'Decimals' ] );
  return( rescale );
};

###########################################################################################
# Routine: get.forward.rates
###########################################################################################

get.forward.rates <- function( 
  symbol,
  forward.dates = NULL,
  pricing.date = today(),
  BidAsk = 'Ask'
) {
  fwd.points = get.forward.points( symbol, forward.dates = forward.dates, pricing.date = pricing.date, BidAsk = BidAsk );

  exchange.rates = get.exchange.rates( symbol, start.date = pricing.date %m-% months(1), end.date = pricing.date %m+% months(1) );
  exchange.rates = fill.missing( exchange.rates ); 
  date.diffs = as.numeric(pricing.date) - as.numeric(as.Date(row.names( exchange.rates ) ) );
  loc = max( which( date.diffs >= 0 ) );
  fx.spot = exchange.rates[loc];

  rescale = get.fx.forward.points.rescale.factor(symbol);
  fwd.rates = data.frame( fwd.points$Forward.Date, fx.spot + fwd.points[,BidAsk]/rescale );
  colnames( fwd.rates ) = c( 'Forward.Date', BidAsk );
  return( fwd.rates );
};

###########################################################################################
# Routine: calc.forward.prices
###########################################################################################

calc.forward.prices <- function(
  symbol,
  forward.dates,
  fwd.contract.rates,
  pricing.date = today(),
  BidAsk = 'Ask'
) {
  fwd.rates = get.forward.rates( symbol, forward.dates = forward.dates, pricing.date = pricing.date, BidAsk = BidAsk );

  prices = numeric( length( forward.dates ) );
  for( i in 1:nrow( fwd.rates ) ) {
    loc = which( forward.dates == fwd.rates$Forward.Date[i] );
    prices = fwd.rates[i,BidAsk] - fwd.contract.rates[loc];
  };
  
  return( prices );
};

###########################################################################################
# Routine: calc.fx.forward.prices.ts
#
# Takes a currency cross symbol (e.g., EUR/USD, USD/JPY) as input, and returns time series
#   of the prices
###########################################################################################

calc.fx.forward.prices.ts <- function(
  symbol,
  forward.date,
  fwd.contract.rate, 
  start.date = today() %m-% years(10),
  end.date = today(),
  BidAsk = 'Mid'
) {
  current.data = read.csv( FORWARD_RATES_RECENT_CSV, sep = '\t', header = TRUE, stringsAsFactors = FALSE );

  # Restrict data frame to relevant currency
  current.data = current.data[ current.data$Symbol == symbol, ];

  # Select appropriate data columns, and create Mid quote if necessary
  if( BidAsk == 'Mid' ) {
    current.data = cbind( current.data, data.frame( Mid = (current.data$Bid + current.data$Ask ) / 2 ) );
  };

  # Remove all but necessary columns
  data = current.data[, c( 'AsOfDate', 'Forward.Date', BidAsk ) ];
  colnames( data ) = c( 'AsOfDate', 'Forward.Date', 'Points' );

  # Get rescale factor, based on number of decimals in forward point quote
  rescale = get.fx.forward.points.rescale.factor(symbol);
  data$Points = data$Points / rescale;

  # Create time series where each column is a different pricing date
  uniq.dates = unique( data$AsOfDate );
  df = c();
  for( i in 1:length(uniq.dates) ) {
    row.data = data[ data$AsOfDate == uniq.dates[i], ];
    interp = approx( as.Date( row.data$Forward.Date ), row.data$Points, forward.date );
    new.df = data.frame( Date = uniq.dates[i], Value = interp$y );
    df = rbind( df, new.df );
  };

  # Convert forward pt data frame to time series
  ts = as.timeSeries(df);

  # Get exchange rates, and merge time series with the forward points
  exchange.rates = get.exchange.rates( symbol, start.date = start.date %m-% months(1), end.date = end.date );
  mts = nanmerge( ts, exchange.rates, rm.na = F );
  mts = fill.missing(mts, end.date = end.date );

  first.val = mts[min( which(!is.na(mts[,1])) ),1];
  mts[is.na(mts[,1]),1] = first.val;

  # Forward rates = ( Spot FX rate ) + ( Forward Points ) / ( Rescale factor )
  forward.rates = mts[,1] + mts[,2];
  forward.prices = forward.rates - fwd.contract.rate;

  # Only keep points between start and end dates
  forward.prices = ts.range( forward.prices, start.date, end.date );

  return( forward.prices );
};

###########################################################################################
# Routine: get.time.series.fx.forwards
###########################################################################################
  
get.time.series.fx.forwards <- function(
  fx.fwd.symbols,
  start.date = today() %m-% years(10),
  end.date = today(),
  BidAsk = 'Mid', 
  target.ccy = 'USD'
) {
  if( is.empty( fx.fwd.symbols ) ) {
    return( NULL )
  };
  
  all.fwd.info = parse.fx.forward.symbols( fx.fwd.symbols );
  
  ts = timeSeries();
  for( i in 1:nrow(all.fwd.info) ) {
    fwd.info = all.fwd.info[i,];

    # Get the FX forward price time series.  This time series is denominated in the bottom currency of the pair.  
    # For example, the forward price for USD/JPY would be denominated in JPY
    raw.ts = calc.fx.forward.prices.ts( symbol = fwd.info$Currency.Cross, forward.date = fwd.info$Settlement.Date, 
             fwd.contract.rate = fwd.info$Settlement.Rate, start.date = start.date, end.date = end.date, BidAsk = BidAsk );

    # Figure out the long and short legs of the forward
    fx.info = parse.currency.pair.symbols( fwd.info$Currency.Cross );

    # Convert the currency of the forward time series to the target currency
    new.ts = convert.ts.currency( raw.ts, from.ccy = fx.info$Long.Currency, to.ccy = target.ccy ); 
    colnames( new.ts ) = fx.fwd.symbols[i];
    ts = merge.ts( ts, new.ts );
  };

  return( ts );
};

###########################################################################################
# Routine: convert.currency
###########################################################################################

convert.currency <- function(
  vals,
  from.ccy,
  to.ccy = 'USD', 
  asOfDate = last.biz.date()
) {
  if( from.ccy == to.ccy ) {
    return( vals );
  } else {
    # Create a time series on the asOfDate
    ts = timeSeries( 1, asOfDate );
 
    # Find time series in new currency 
    spot.rate = convert.ts.currency( ts, from.ccy = from.ccy, to.ccy = to.ccy );
  
    # Convert result back to an array, and return the result
    new.vals = vals * as.numeric(spot.rate);
    return( new.vals );
  };
};

###########################################################################################
# Routine: convert.ts.currency
###########################################################################################

convert.ts.currency <- function(
  ts, 
  from.ccy, 
  to.ccy = 'USD'
) {
  # Get Exchange rate
  fx = get.exchange.rates( from.ccy = from.ccy, to.ccy = to.ccy, start.date = first.date(ts) - 7, end.date = last.date(ts) + 7 );
  fx = fill.missing( fx, end.date = last.date(ts) + 7 );
  fx = ts.range( fx,  start.date = first.date(ts), end.date = last.date(ts) );

  # Fill missing values
  mts = fill.missing( merge.ts( ts, fx ) );
  # Multiply FX rate by original time series
  new.ts = mts[,1] * mts[,2];
  # Keep the original column name and original dates
  colnames(new.ts) = colnames(ts);
  new.ts = new.ts[ rownames(ts), ];
  return( new.ts );
};


