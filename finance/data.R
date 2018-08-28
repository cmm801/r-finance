library( "reshape", quietly = TRUE );

###########################################################################################
# Routine: get.time.series
#
# Takes time series data from Yahoo! and formats and aligns it so each row contains prices
# from each of the individual time series.
###########################################################################################

get.time.series = function(
  symbols,
  start.date = today() %m-% years(10), 
  end.date   = last.biz.date( today() - 1 ),
  data.type  = 'AdjClose', 
  BidAsk     = 'Ask', 
  rm.na      = TRUE, 
  use.proxy  = FALSE
) {
  symbol.list = get.asset.class.list( symbols );
  if( !is.empty(symbol.list$Other) ) {
    print( sprintf( 'Unclassified asset class for symbol %s', symbol.list$Other[1] ) );
  };

  # Equity & Index data
  equity.index.symbols = c( symbol.list$Equity, symbol.list$Index );
  equity.ts = get.time.series.equity( equity.index.symbols, start.date = start.date, end.date = end.date, 
								data.type = data.type, rm.na = rm.na, use.proxy = use.proxy );
  # Options - use proxies all the time for Options
  option.ts = get.time.series.options( symbol.list$Option, start.date = start.date, end.date = end.date, 
								BidAsk = BidAsk, use.proxy = TRUE );
  # Cash / Money Markets
  cash.ts = get.time.series.cash( symbol.list$Cash, start.date = start.date, end.date = end.date );

  # Exchange Rates
  ccy.pair.ts = get.time.series.fx( symbol.list$CurrencyPair, start.date = start.date, end.date = end.date );

  # FX Forwards
  fx.fwd.ts = get.time.series.fx.forwards( symbol.list$CurrencyForward, start.date = start.date, 
									end.date = end.date, BidAsk = BidAsk );

  # If there are equity, option, or forward time series, only return cash & FX values
  #    on the dates on which the tradable asset class data is available
  non.cash.ts = merge.ts( list( equity.ts, option.ts, fx.fwd.ts ) );
  cash.fx.ts = merge.ts( list( cash.ts, ccy.pair.ts ) );
  if( !is.empty( non.cash.ts ) ) {
    if( !is.empty( cash.fx.ts ) ) {
      cash.fx.ts = fill.missing( cash.fx.ts, start.date = first.date(non.cash.ts), end.date = last.date(non.cash.ts) );
      #all.ts = merge.ts( list( non.cash.ts, cash.fx.ts[ rownames( non.cash.ts ), ] ) );
      all.ts = merge.ts( list( non.cash.ts, cash.fx.ts ) );
      all.ts = all.ts[ , name2label( symbols ) ];
      all.ts = ts.range( all.ts, start.date = start.date, end.date = end.date );
      all.ts = na.locf( all.ts );
      return( all.ts );
    } else {
      non.cash.ts = ts.range( non.cash.ts, start.date = start.date, end.date = end.date );
      return( non.cash.ts );
    };
  } else {
    cash.fx.ts = ts.range( cash.fx.ts, start.date = start.date, end.date = end.date );
    return( cash.fx.ts );
  };
};

###########################################################################################
# Routine: get.time.series.equity
#
# Get equity and equity proxy time series
###########################################################################################

get.time.series.equity = function(
  symbols,
  start.date = today() %m-% years(10),
  end.date   = last.biz.date( today() - 1 ),
  data.type  = 'AdjClose',
  rm.na      = TRUE,
  use.proxy  = FALSE
) {
  if( is.empty( symbols ) ) {
    return( NULL )
  };

  # Get Equity time series for symbols that are not proxied
  equity.orig.ts = get.db.data( symbols, start = start.date, Table = 'EquityData',
                                                           value.name = data.type, rm.na = rm.na );

  if( is.null( equity.orig.ts ) ) {
    equity.orig.ts = get.last.price( symbols );
  } else {
    # Add intraday price if necessary
    last.db.date = last.date( equity.orig.ts );
    if( last.db.date < end.date ) {
      last.prices = get.last.price( symbols );
      if( min( as.Date( rownames( last.prices ) ) ) > last.db.date ) {
         equity.orig.ts = merge( equity.orig.ts, last.prices );
      }
    };
  };
  
  equity.orig.ts = equity.orig.ts[, name2label( symbols ) ];

  # Get Equity Proxies, and then add them to existing time series
  if( use.proxy ) {
    equity.proxy.ts = get.etf.proxy.ts( symbols, start.date = start.date, rm.na = rm.na );
    ts = list();
    for( i in 1:ncol( equity.orig.ts ) ) {
      label = colnames( equity.orig.ts )[i];

      if( label %in% colnames( equity.proxy.ts ) ) {
        orig = na.omit( equity.orig.ts[, label ] );
        proxy = na.omit( equity.proxy.ts[, label ] );
  
        first.common.date = as.Date( intersect( rownames( orig ), rownames( proxy ) )[1] );
        rescale = as.numeric( orig[ first.common.date, ] ) / as.numeric(  proxy[ first.common.date, ] );
        proxy = proxy[ rownames( proxy ) < rownames( orig[1,] ), ] * rescale;

        if( is.empty( proxy ) ) {
          ts[[i]] = orig;
        } else if( is.empty( orig ) ) {      
          ts[[i]] = proxy;
        } else {
          ts[[i]] = merge( proxy, orig ); 
        };
      } else { 
        ts[[i]] = equity.orig.ts[, label ];
      };
    };
   
    equity.ts = merge.ts( ts );
  } else {
    equity.ts = equity.orig.ts;
  };

  # Remove NA's if rm.na is TRUE
  if( rm.na ) {
    equity.ts = equity.ts[ !is.na( rowSums( equity.ts ) ), ];
  };

  # Restrict time series to be between start/end dates
  equity.ts = ts.range( equity.ts, start.date = start.date, end.date = end.date );

  return( equity.ts );
};

###########################################################################################
# Routine: get.etf.proxy.ts
###########################################################################################

get.etf.proxy.ts <- function( 
  Symbols = NULL, 
  start.date = today() %m-% years(10), 
  end.date = last.biz.date(), 
  rm.na = TRUE 
) {
  proxy.ts = get.proxy.ts( Symbols, start.date = start.date, end.date = end.date, 'ETF', rm.na = rm.na );
  return( proxy.ts );
};  

###########################################################################################
# Routine: get.proxy.ts
# 
# Read stored proxy ETF or Option time series from csv 
###########################################################################################

get.proxy.ts <- function( 
  Symbols = c(), 
  start.date = today() %m-% years(10), 
  end.date = last.biz.date(),
  type, 
  rm.na = TRUE
) {
   # Read the ETF/Option data from .csv files
   if( type == 'ETF' ) {
     data = read.csv( ETF_PROXIES_CSV, stringsAsFactors = FALSE, sep = "\t", header = TRUE );
     proxy.symbols = intersect( Symbols, colnames( data ) );
     labels = proxy.symbols;
   } else if( type == 'Option' ) {
     data = read.csv( OPTION_PROXIES_CSV, stringsAsFactors = FALSE, sep = "\t", header = TRUE );
     idx = match.option.symbols( Symbols, colnames(data) );
     proxy.symbols = colnames( data )[idx[!is.na(idx)]];
     labels = Symbols[ !is.na(idx) ];
   } else { 
     stop( paste( 'Unsupported type for get.proxy.ts:', type ) );
   };

   data = timeSeries( data );

   # If empty, return NULL
   if( !is.empty( proxy.symbols ) ) {
     data = data[, proxy.symbols ];
   } else { 
     return( NULL );
   };

   # Remove NA's if required   
   if( rm.na == TRUE ) {
     data = data[ !is.na( rowSums( data ) ), ];
   };

   # Restrict the range of the option data
   data = ts.range( data, start.date, end.date );

   # Put the columns in the right order
   data = data[, proxy.symbols ];

   # Rename the columns (in case they are options with different as-of-dates)
   colnames(data) = labels;

   return( data );
};

###########################################################################################
# Routine: get.time.series.cash
#
# Convert the interest rate into a time series.  
###########################################################################################

get.time.series.cash <- function( 
  cash.symbols, 
  start.date = today() %m-% years(10), 
  end.date   = last.biz.date( today() ), 
  to.ccy = NA
) {
  if( is.empty( cash.symbols ) ) {
    return( NULL );
  };

  ts = c();
  for( i in 1:length(cash.symbols) ) {
    if( substr( cash.symbols[i], 4, 10 ) != 'Cash' ) {
      stop( sprintf( 'Invalid cash symbol: %s', cash.symbols[i] ) );
    };

    currency = substr( cash.symbols[i], 1, 3 );
    interest.rate <- get.interest.rate( currency, start.date = start.date, end.date = end.date );
    cash.ts <- cumulated( interest.rate / 360, method='discrete' );
    if( !is.na( to.ccy ) ) {
      cash.ts.usd <- convert.ts.currency( cash.ts, from.ccy = currency, to.ccy = to.ccy );
      ts = merge.ts( ts, cash.ts.usd );
    } else {
      ts = merge.ts( ts, cash.ts );
    };
  };

  colnames( ts ) = cash.symbols;
  return( ts );
};

###########################################################################################
# Routine: get.interest.rates
#
# Download Interest Rate information from the Fed database
###########################################################################################

get.interest.rates <- function(
  currencies = 'USD',
  n.months = 1,
  start.date = today() %m-% years( 50 ),
  end.date   = last.biz.date( today() - 1 )
) {
  ts = c();
  for( i in 1:length( currencies ) ) {
    new.ts = get.interest.rate( currency = currencies[i], n.months = n.months, 
						start.date = start.date, end.date = end.date );
    if( i == 1 ) {
      ts = new.ts;
    } else {
      ts = merge.ts( ts, new.ts );
    };
  };

  return( ts );  
};

###########################################################################################
# Routine: get.interest.rate 
#
# Download Interest Rate information from the Fed database
###########################################################################################

get.interest.rate <- function( 
  currency = 'USD', 
  n.months = 1, 
  start.date = today() %m-% years( 50 ), 
  end.date   = last.biz.date( today() - 1 )
) {
  if( n.months %in% c(1,3,6) ) {
    symbol = sprintf( '%s%dMTD156N', currency, n.months );
  } else if( n.months == 12 ) {
    symbol = sprintf( '%s12MD156N', currency );
  } else {
    stop( sprintf( 'Interest rate info not supported for n.months = %d', n.months ) );
  };

  data = get.fed.data( symbol, start.date = start.date - 30, end.date = end.date ) / 100;
 
  # Remove 0's from data, as there are bad points in some of the FRED interest rate curves
  data = data[ data !=0, ];

  # Fill in missing
  data = fill.missing( data, end.date = end.date );

  # Restrict the time series to lie between the start/end dates
  data = ts.range( data, start.date = start.date, end.date = end.date );

  # Replace column name with currency
  colnames( data ) = currency;

  return( data );
};

###########################################################################################
# Routine: get.time.series.options
###########################################################################################

get.time.series.options <- function(
  Symbols,
  Strike        = NA,
  yearMonth     = NA,
  Expiration.Date = NA,
  Type          = NA,
  start.date    = today() %m-% years(1),
  end.date      = last.biz.date( today() ),
  use.proxy     = FALSE,
  BidAsk        = 'Ask'
) {
  if( is.empty( Symbols ) ) {
    return( NULL );
  };

  all.option.ts = list();
  for( k in 1:length( Symbols ) ) {
    Symbol = Symbols[k];

    if( is.option.symbol( Symbol ) ) {
      Option.Symbol = Symbol;
      info = parse.option.symbol( Symbol );
      underlier.symbol = info$Symbol;
      Strike = info$Strike;
      Expiration.Date = info$Expiration.Date;
      Type = info$Type;
    } else {  
      underlier.symbol = Symbol;
      Expiration.Date = yearMonth2expiry( yearMonth );
      Option.Symbol = create.option.symbol( Symbol, Strike = Strike, Type = Type, Expiration.Date = Expiration.Date );
    };

    db.ts = get.db.option.ts( underlier.symbol, Strike = Strike, Expiration.Date = Expiration.Date, Type = Type, BidAsk = BidAsk );
    db.ts = db.ts[ !is.na( db.ts ), ];

    # Use proxy if specified by input argument
    if( use.proxy ) {
      option.ts = get.option.proxy.ts( Option.Symbol, start.date = start.date, end.date = end.date, rm.na = TRUE );
    } else {
      option.ts <- db.ts;
    };

    # Make sure not to include data after the expiration Date
    dates = as.Date( rownames( option.ts ) );

    option.ts <- timeSeries( as.numeric( option.ts )[ dates <= Expiration.Date ], dates[ dates <= Expiration.Date ], Option.Symbol );
    all.option.ts[[k]] = option.ts;
  };

  ts = merge.ts( all.option.ts );
  return( ts )
};

###########################################################################################
# Routine: get.fed.data
#
# Get Fed data from the FRED database
###########################################################################################

get.fed.data <- function( 
 Symbols, 
 start.date = today() %m-% years(100), 
 end.date   = last.biz.date(), 
 rm.na      = FALSE
) {
  ts = get.db.data( Symbols, start = start.date, end = end.date, value.name = 'Value', Table = 'FREDData', rm.na = rm.na );
  return( ts ); 
};

###########################################################################################
# Routine: get.rt.data
###########################################################################################

get.rt.data <- function( 
  symbols, 
  start.date = last.biz.date() - 1,
  end.date   = last.biz.date(),
  asOfDate   = NULL,
  rm.na      = TRUE
) {
  start.time = as.POSIXct( format( start.date, '%Y-%m-%d 00:00:00' ) ); 
  end.time   = as.POSIXct( format( end.date,   '%Y-%m-%d 23:59:59' ) ); 

  rt.ts = get.db.data( symbols, start = start.time, end = end.time, value.name = 'Price', date.name = 'Trade_Time', 
									rm.na = rm.na, Table = 'IntradayEquity' )

  return( rt.ts ); 
};

###########################################################################################
# Routine: get.dividends
###########################################################################################

get.dividends = function( 
  symbols,
  start.date = Sys.Date() %m-% years(10),
  end.date   = Sys.Date()
) {
  all.dividends = read.table( DIVIDENDS_CSV, header = TRUE, stringsAsFactors = FALSE );
  dividends = all.dividends[ all.dividends$Symbol %in% symbols, ];
  dividends = dividends[ start.date <= dividends$Date & dividends$Date <= end.date, ];
  return( dividends );
}

###########################################################################################
# Routine: get.dividends.ts
###########################################################################################

get.dividends.ts <- function( 
  symbols, 
  start.date = Sys.Date() - 365,
  end.date   = Sys.Date()
) {
  divs = get.dividends( symbols = symbols, start.date = start.date, end.date = end.date );

  if( nrow( divs ) == 0 ) {
    div.ts = timeSeries()
  } else {
    # Rearrange data so that each Symbol has its own column
    rcst = recast( divs, Date ~ Symbol, value = 'Dividends', id.var = c( 'Symbol', 'Date' ), fun.aggregate = function(x) x[1] );
  
    # Replace NA's with zeros
    rcst[ is.na( rcst ) ] = 0;

    # Convert matrix data into a time series
    div.ts = timeSeries( rcst[, -1 ], rcst$Date, units = colnames( rcst )[-1] );
  };

  return( div.ts );
};

###########################################################################################
# Routine: yearMonth2expiry
###########################################################################################

yearMonth2expiry <- function( yearMonth ) {
  return( get.expiries( 'US Options', start.ym = yearMonth, end.ym = yearMonth ) );
};

###########################################################################################
# Routine: get.option.data
#
# Download option data from MySQL or calculate the implied probability
###########################################################################################

get.option.data <- function( 
  Symbols, 
  yearMonths = c(),
  asOfDate   = last.option.date( Symbols ), 
  DB         = MYSQL_DB_NAME
) {
  Table = "YahooOptions";

  symbol.list = paste( Symbols, collapse = "', '" );
  whereYearMonths = ifelse( length( yearMonths ) == 0, "", paste( "AND yearMonth IN (", paste( yearMonths, collapse = ", " ), ")" ) );

  query    = sprintf( "SELECT * FROM %s.%s WHERE Symbol in ('%s') AND Date = '%s' %s", DB, Table, symbol.list, asOfDate, whereYearMonths );
  opt.data = run.query( query, DB = DB );
   
  missing.symbols = setdiff( Symbols, unique( opt.data$Symbol ) );
  if( length( missing.symbols ) > 0 ) {
    print( sprintf( 'Missing option data for symbols %s', paste( missing.symbols, sep = '', collapse = ', ' ) ) );
  };
  
  return( opt.data );
};

