
library( XML );
library( lubridate );

source( paste( HOME, "/programming/R/database/download.R", sep = "" ) );

###########################################################################################
# Routine: download.investing.com.exchange.rates 
#
# Get recent FX information from investing.com
###########################################################################################

download.investing.com.exchange.rates <- function( from, to ) {
  
  url = sprintf( 'https://www.investing.com/currencies/%s-%s-historical-data', from, to );
  test.url = tryCatch( readLines( url ), error = function(e)  NULL );

  raw.text = readLines( url );
  tables = readHTMLTable( raw.text, stringsAsFactors = FALSE );

  exchange.rates = tables$curr_table;
  exchange.rates = exchange.rates[ , c( 'Date', 'Price' ) ];

  new.dates = numeric(nrow(exchange.rates));
  for( i in 1:nrow(exchange.rates) ) {
    date = exchange.rates$Date[i];
    M = which( month.abb == substr( date, 1, 3 ) );
    D = substr( date, 5, 6 );
    Y = substr( date, 9, 12 );

    new.dates[i] = sprintf( '%s-%d-%s', Y, M, D );
  };
  
  exchange.rates$Date = format(as.Date( new.dates ));
  return( exchange.rates );
};


###########################################################################################
# Routine: download.all.investing.com.exchange.rates 
#
# Get recent FX information from investing.com
###########################################################################################

download.all.investing.com.exchange.rates <- function( n.retry = 3, sleep.time = 2 ) {
  fx.info = read.csv( EXCHANGE_RATE_INFO_CSV, header = TRUE, stringsAsFactors = FALSE );

  data = c();
  for( i in 1:nrow( fx.info ) ) {
    currency = fx.info$Currency[i];
    print( sprintf( 'Downloading data from investing.com for %s...', currency ) );

    # Parse the cross symbol (e.g., EUR/USD), to get the 'from' and 'to' currencies
    symbol = fx.info$Symbol[i];
    currencies = strsplit( symbol, '/' );
    from.ccy = currencies[[1]][1];
    to.ccy = currencies[[1]][2];
    args = list( from = from.ccy, to = to.ccy );

    ex.rate = run.with.retry( download.investing.com.exchange.rates, args, n.retry = n.retry, sleep.time = sleep.time );
    if( !is.null( ex.rate ) ) {
      data = rbind( data, cbind( ex.rate, data.frame( Currency = currency ) ) );
    };
  };

  return( data );
};

###########################################################################################
# Routine: download.investing.com.forward.rates 
#
# Get recent FX information from investing.com
###########################################################################################

download.investing.com.forward.rates <- function( from, to ) {
  options( stringsAsFactors = FALSE );
 
  url = sprintf( 'https://www.investing.com/currencies/%s-%s-forward-rates', from, to );
  test.url = tryCatch( readLines( url ), error = function(e)  NULL );

  raw.text = readLines( url );
  tables = readHTMLTable( raw.text, stringsAsFactors = FALSE );
  forward.rates = tables$curr_table;
  asOfDate = Sys.Date();
  if( wday( asOfDate ) == 7 ) {
    asOfDate = asOfDate - 1;
  } else if( wday( asOfDate )  == 1 ) {
    asOfDate = asOfDate - 2;
  };

  # Parse the Name column to get the forward dates
  end.dates = c();
  for( i in 1:nrow(forward.rates) ) {
    tenorString = substr(forward.rates$Name[i], 8, nchar(forward.rates$Name[i])-4);
    period  = gsub( '[0-9]', '', tenorString );
    duration  = as.numeric( gsub( '[A-Za-z]', '', tenorString ) );
    if( period == 'ON' ) { 
      # Overnight
      end.dates[i] = asOfDate + 1;
    } else if( period == 'TN' ) {
      # Tomorrow Next
      end.dates[i] = asOfDate + 1;
    } else if( period == 'SN' ) {  
      # Spot Next
      end.dates[i] = asOfDate + 3;
    } else if( period == 'SW' ) {
      # Spot Week
      end.dates[i] = asOfDate + 7;
    } else if( period == 'W' ) {
      end.dates[i] = asOfDate + weeks(duration);
    } else if( period == 'M' ) {
      end.dates[i] = asOfDate + months(duration);
    } else if( period == 'Y' ) {
      end.dates[i] = asOfDate + years(duration);
    } else {
      stop( sprintf( 'Error: unknown period type - %s', period ) );
    };
  };
 
  output = cbind( data.frame( AsOfDate = format(asOfDate), 'Forward Date' = 
	format(as.Date(end.dates,origin=origin) )), forward.rates[, c( 'Bid', 'Ask' ) ] );

  return( output );
};


###########################################################################################
# Routine: download.all.investing.com.forward.rates
#
# Get recent FX information from investing.com
###########################################################################################

download.all.investing.com.forward.rates <- function( n.retry = 3, sleep.time = 0.1 ) {
  fx.info = read.csv( EXCHANGE_RATE_INFO_CSV, header = TRUE, stringsAsFactors = FALSE );

  data = c();
  for( i in 1:nrow( fx.info ) ) {
    currency = fx.info$Currency[i];
    print( sprintf( 'Downloading data from investing.com for %s...', currency ) );

    # Parse the cross symbol (e.g., EUR/USD), to get the 'from' and 'to' currencies
    symbol = fx.info$Symbol[i];
    currencies = strsplit( symbol, '/' );
    from.ccy = currencies[[1]][1];
    to.ccy = currencies[[1]][2];
    args = list( from = from.ccy, to = to.ccy );

    fwd.rate = run.with.retry( download.investing.com.forward.rates, args, n.retry = n.retry, 
						sleep.time = sleep.time, throw.error = FALSE );
    if( !is.null( fwd.rate ) ) {
      data = rbind( data, cbind( fwd.rate, data.frame( Currency = currency, Symbol = symbol ) ) );
    };
  };

  return( data );
};
