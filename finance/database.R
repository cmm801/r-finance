
library( XML, quietly = TRUE );
library( quantmod, quietly = TRUE );
library( RMySQL, quietly = TRUE );
library( timeSeries, quietly = TRUE );
library( lubridate, quietly = TRUE );

###########################################################################################
# Routine: update.all.data
#
# THis will download all updated data and upload it into the database, or into CSV files. 
###########################################################################################

update.all.data <- function() {
  
  print( 'Updating portfolio symbol lists...' );
  update.pf.symbol.lists();
 
  print( 'Updating Betas...' );
  #update.beta();

  print( 'Updating Risk Factors...' );
  update.db.factors();

  print( "Updating ETF proxies..." );
  save.etf.proxies();

  print( "Updating Option proxies..." );
  save.option.proxies();

  print( 'Updating ETF Alpha...' );
  save.etf.alphas();
};


##########################################################################################################
# Routine: update.db.factors
##########################################################################################################

update.db.factors <- function() {
  # Update Fama-French Factors
  upload.ff.data();

  # Monthly Stambaugh Liquidity Factor
  upload.stambaugh.liquidity.factor();

  # Daily and Monthly Oil Factor
  copy.factor.data();

  # SPXVol Factor
  upload.spxvol.factor();

  # TERM Factor
  upload.term.factor();
};

###########################################################################################
# Routine: report.db.status
###########################################################################################

report.db.status <- function( ndays = 7 ) {
  reporting.dates = as.Date((today()-ndays+1):today());
  reporting.dates = reporting.dates[!( wday(reporting.dates) %in% c(7,1) ) ];

  table.data = read.csv( TABLE_DATE_INFO_CSV, sep=",", header = TRUE );
  tables = table.data$Table;

  all.info = data.frame(matrix(NA,ncol=length(reporting.dates),nrow=nrow(table.data) ) );
  colnames(all.info) = format( reporting.dates );
  rownames(all.info) = table.data$Table;

  for( i in 1:nrow( table.data ) ) {
    table     = table.data[i,'Table'];
    date.col  = table.data[i,'DateColumn'];
    date.type = table.data[i,'DateType'];

    date.counts = run.query( sprintf( 'SELECT %s, COUNT(*) as Count from %s Group by %s', date.col, table, date.col ) );
    if( date.type == 'Date' ) {
      all.info[i,] = date.counts[ match( reporting.dates, as.Date(date.counts[,date.col] ) ), 'Count' ];
    } else if( date.type == 'DateTime' ) {
      trade.dates = as.Date( date.counts$Trade_Time );
      for( k in 1:length( reporting.dates ) ) {
        reporting.date = as.Date( reporting.dates[k] );
        all.info[i,k] = sum( date.counts[ trade.dates == reporting.date, 'Count' ] );
      };
    } else { 
      stop( paste( 'Unknown date type: ', date.type ) );
    };
  };

  return( all.info ); 
};

###########################################################################################
# Routine: save.etf.alphas
###########################################################################################

save.etf.alphas <- function() {
  output = calc.etf.tracking.error();
  write.table( output, file = ETF_ALPHA_CSV, sep = '\t', row.names = F, col.names = T, quote = F );
};

###########################################################################################
# Routine: get.etf.alphas
###########################################################################################

get.etf.alphas <- function( symbols = NULL ) {
  etf.alphas = read.table( ETF_ALPHA_CSV, header = TRUE, stringsAsFactors = FALSE );
  if( !is.null( symbols ) ) {
    etf.alphas = etf.alphas[ etf.alphas$Symbol %in% symbols, ]
  };
   
  return( etf.alphas );
};


###########################################################################################
# Routine: get.time.series.fx
###########################################################################################

get.time.series.fx <- function( 
  symbols,
  start.date = today() %m-% years(30),
  end.date = today()
) {
  if( is.empty( symbols ) ) {
    return( NULL );
  };

  parsed = strsplit( symbols, '/' );
  ts = c();
  for( i in 1:length(parsed) ) {
    from.ccy = parsed[[i]][1];
    to.ccy = parsed[[i]][2];
    new.ts = get.exchange.rates( from.ccy = from.ccy, to.ccy = to.ccy, start.date = start.date, end.date = end.date );
    ts = merge.ts( ts, new.ts );
  };
  
  return( ts );
};

###########################################################################################
# Routine: get.exchange.rates
#
# Get exchange rate from one currency to another
# If the 'to' currency is NULL and is not a currency cross (like USD/JPY), then convert to USD
###########################################################################################

get.exchange.rates <- function( 
  from.ccy, 
  to.ccy = NULL,
  start.date = today() %m-% years(30), 
  end.date = today() 
) {

  fx.rates = NULL;

  N = max( length(from.ccy), length(to.ccy) );
  for( i in 1:length(from.ccy) ) {

    if( length(to.ccy) < 2 ) {
      fx.rate = get.single.exchange.rate( from.ccy[i], to.ccy = to.ccy, start.date = start.date, end.date = end.date );
    } else if ( length(from.ccy) == 1 ) {
      fx.rate = get.single.exchange.rate( from.ccy, to.ccy = to.ccy[i], start.date = start.date, end.date = end.date );
    } else {
      fx.rate = single.exchange.rate( from.ccy[i], to.ccy = to.ccy[i], start.date = start.date, end.date = end.date );
    };

    fx.rates = nanmerge( fx.rates, fx.rate, rm.na = FALSE );
  };
  
  return(fx.rates);
};

###########################################################################################
# Routine: get.single.exchange.rate
#
# Get exchange rate from one currency to another
# If the 'to' currency is NULL and is not a currency cross (like USD/JPY), then convert to USD
###########################################################################################

get.single.exchange.rate <- function(
  from.ccy,
  to.ccy = NULL,
  start.date = today() %m-% years(30),
  end.date = today()
) {

  # If the to and from currency are the same, the exchange rate is always 1
  if( ( !is.null(to.ccy) && from.ccy == to.ccy ) || ( is.null(to.ccy) && from.ccy == 'USD' ) ) {
    dates = get.all.biz.days( start.date, end.date );
    ts = timeSeries( rep(1,length(dates)), dates, units = paste( from.ccy, to.ccy, sep = '/' ) );
    return( ts );
  };

  fx.info = read.csv( EXCHANGE_RATE_INFO_CSV, header = TRUE, stringsAsFactors = FALSE );
 
  # If the user provided a currency cross (e.g., USD/JPY), split this into to/from currency 
  if( is.null(to.ccy) && any(grep( '/', from.ccy )) ) {
    to.ccy   = strsplit( from.ccy, '/' )[[1]][2];
    from.ccy = strsplit( from.ccy, '/' )[[1]][1];
  };

  if( !is.null(to.ccy) && from.ccy != 'USD' && to.ccy != 'USD' ) {
    fx1 = get.exchange.rates( from.ccy, 'USD', start.date = start.date, end.date = end.date );
    fx2 = get.exchange.rates( 'USD', to.ccy, start.date = start.date, end.date = end.date );
    fx.combined = merge.ts( fx1, fx2 );
    fx.rate = fx.combined[,1] * fx.combined[,2];
    names(fx.rate) = paste( from.ccy, to.ccy, sep = '/' );
    return( fx.rate );
  } else {
    if( is.null(to.ccy) || to.ccy == 'USD' ) {
      non.us.ccy = from.ccy;
    } else {
      non.us.ccy = to.ccy;
    };

    ccy.symbol = fx.info[ fx.info$Currency %in% non.us.ccy, 'Symbol' ];
    fed.symbol = fx.info[ fx.info$Currency %in% non.us.ccy, 'FRED.Symbol' ]

    fed.ts = get.fed.data( fed.symbol, start.date = start.date );	
    fed.ts = fed.ts[ fed.ts[,fed.symbol] != 0, ];

    recent.fx = read.csv( EXCHANGE_RATES_RECENT_CSV, sep="\t", header = T, stringsAsFactors = F );
    recent.fx = recent.fx[ recent.fx$Currency == non.us.ccy, ];
    recent.ts = timeSeries( recent.fx$Price, as.Date( recent.fx$Date ), units = fed.symbol );

    if( !is.null( fed.ts ) ) {
      recent.ts = ts.range( recent.ts, start.date = last.date(fed.ts) + 1, end.date = end.date );
      if( !is.empty( recent.ts ) ) {
        fx.rate = merge( fed.ts, recent.ts );
      } else {
        fx.rate = fed.ts;
      };
    } else {
      fx.rate = sort( as.timeSeries( recent.ts ) );
    };

    parsed = strsplit( ccy.symbol, '/' );
    if( !is.null(to.ccy) && ( parsed[[1]][1] != from.ccy || parsed[[1]][2] != to.ccy ) ) {
      fx.rate = 1 / fx.rate;
    };
   
    if( is.null( to.ccy ) ) {
      to.ccy = 'USD';
    };

    colnames(fx.rate) = paste( from.ccy, to.ccy, sep = '/' ); 
    fx.rate = ts.range( fx.rate, start.date = start.date, end.date = end.date );
    return(fx.rate);
  };
};

###########################################################################################
# Routine: upload.data 
###########################################################################################

upload.data <- function( data, DB = MYSQL_DB_NAME, Table ) {
  con = get.db.connection( DB = DB );

  dbWriteTable( con, Table, data, overwrite = FALSE, append = TRUE, row.names = FALSE );
  dbDisconnect( con );
};

###########################################################################################
# Routine: get.db.connection
###########################################################################################

get.db.connection <- function( DB = MYSQL_DB_NAME ) {
  drv = dbDriver('MySQL')
  if( MYSQL_SOCKET != '' ) {
    con = dbConnect(drv,db = DB, user = 'root', pass = MYSQL_PASSWORD, unix.socket = MYSQL_SOCKET );
  } else {
    con = dbConnect(drv,db = DB, user = 'root', pass = MYSQL_PASSWORD );
  };

  return( con );
};

###########################################################################################
# Routine: get.all.db.data
#
###########################################################################################

get.all.db.data <- function(
  Symbols,
  start         = today() %m-% years(3),
  end           = last.biz.date( today() - 1 ),
  date.name     = 'Date',
  symbol.name   = 'Symbol', 
  DB            = MYSQL_DB_NAME,
  Table         = '', 
  Where	        = ''
) {
  symbol.list = paste( Symbols, collapse = "', '" );
  if( Where != '' ) {
    Where = paste( 'AND ', Where, sep = '' );
  };

  query = sprintf( "SELECT * FROM %s.%s WHERE %s in ('%s') AND %s >= '%s' AND %s <= '%s' %s",
                                           DB, Table, symbol.name, symbol.list, date.name, start, date.name, end, Where );

  result = run.query( query, DB = DB );

  if( nrow( result ) > 1 ) {
    result = result[ order( result[, date.name ] ), ];
  };

  return( result );
};

###########################################################################################
# Routine: get.db.data
#
###########################################################################################

get.db.data <- function(
  Symbols,
  start         = today() %m-% years(3),
  end           = last.biz.date( today() - 1 ),
  value.name    = "AdjClose",
  date.name     = 'Date',
  symbol.name   = 'Symbol',
  DB            = MYSQL_DB_NAME,
  Table         = '',
  rm.na         = TRUE, 
  Where         = ''
) {
  result = get.all.db.data( Symbols, start=start, end=end, date.name = date.name, symbol.name = symbol.name, 
				DB = DB, Table = Table, Where = Where );

  if( nrow( result ) == 0 ) {
    return( NULL );
  };

  result <- result[ , c( symbol.name, date.name, value.name ) ];
  result[, symbol.name ] = name2label( result[, symbol.name ] );

  # Create a formula for Date ~ Symbol, using the appropriate date.name and symbol.name
  fmla = eval( parse( text=sprintf( "formula( %s ~ %s )", date.name, symbol.name ) ) );
  recast.result = recast( result, fmla, value = value.name, id.var = c( symbol.name, date.name ), fun.aggregate = function(x) x[1] )

  if( 'USDCash' %in% Symbols ) {
    recast.result = cbind( recast.result, data.frame( USDCash = rep( 1, nrow( recast.result ) ) ) );
  };

  new.sym = intersect( colnames( recast.result ), name2label( Symbols ) );
  new.ts = timeSeries( recast.result[, name2label( new.sym ) ], 
					recast.result[, date.name ], units = name2label( new.sym ) );
  
  if( rm.na && !identical( new.ts, c() ) ) {
    new.ts = new.ts[ !is.na( rowSums( new.ts ) ), ];
  };

  return( new.ts )
};


###########################################################################################
# Routine: get.existing.symbols
###########################################################################################

get.existing.symbols <- function( Symbols = NA, DB = MYSQL_DB_NAME, Table = "EquityData" ) {
  symbol.list = paste( Symbols, collapse = "', '" );

  where = ifelse( identical( Symbols, NA ), "", sprintf( "WHERE Symbol IN ( '%s' )", symbol.list ) );
  query = sprintf( "SELECT Distinct Symbol FROM %s.%s %s", DB, Table, where );
  
  result = run.query( query, DB = DB );

  ifelse( nrow( result ) > 0 , return( result$Symbol ), return( c() ) );
};

###########################################################################################
# Routine: run.query
###########################################################################################

run.query <- function( query, DB = MYSQL_DB_NAME ) {
  con = get.db.connection( DB = DB );
  res = dbGetQuery(con, statement = query );
  dbDisconnect( con );

  return( res );
};

###########################################################################################
# Routine: last.db.date
###########################################################################################

last.db.date <- function( Symbols = NA, DB = MYSQL_DB_NAME, Table = "EquityData" ) {
  where  = ifelse( identical( Symbols, NA ), "", sprintf( "WHERE Symbol in ( '%s' )", paste( Symbols, collapse = "', '" ) ) );
  query  = sprintf( "SELECT Symbol, MAX( Date ) as Date FROM %s.%s %s GROUP BY Symbol", DB, Table, where );
  result = run.query( query, DB = DB );
  return( result );
};

###########################################################################################
# Routine: last.option.date
###########################################################################################

last.option.date <- function( Symbols ) { 
  return( as.Date( min( last.db.date( Symbols, DB = MYSQL_DB_NAME, Table = 'YahooOptions' )$Date ) ) );
};

###########################################################################################
# Routine: save.etf.proxies
###########################################################################################

save.etf.proxies <- function() {
  ts = create.all.etf.proxies( recalculate = TRUE );
  write.table( ts, file = ETF_PROXIES_CSV, sep = '\t', row.names = TRUE, quote = FALSE )
};

###########################################################################################
# Routine: save.option.proxies
###########################################################################################

save.option.proxies <- function( transaction.log = TRANSACTIONS_CSV ) {
  quantities = get.pf.quantities( transaction.log = transaction.log );
  symbols = colnames( quantities );

  option.symbols = symbols[ is.option.symbol( symbols ) ];

  ts = create.option.proxies( option.symbols, start.date = first.date( quantities ), 
						end.date = today(), BidAsk = 'Mid' );

  write.table( ts, file = OPTION_PROXIES_CSV, sep = '\t', row.names = TRUE, quote = FALSE )
};


###########################################################################################
# Routine: get.closest.db.option.symbol
###########################################################################################

get.closest.db.option.symbol <- function( 
Option.Symbol, 
start.date,
end.date
) {
  info = parse.option.symbol( Option.Symbol );

  yearMonth = year( info$Expiration.Date ) * 100 + month( info$Expiration.Date );

  query =  sprintf( "SELECT Distinct Strike FROM YahooOptions WHERE Symbol = '%s' AND yearMonth = %d AND Type = '%s' AND Date > '%s' AND Date <= '%s'", 
 		          info$Symbol, yearMonth, info$Type, as.Date( start.date ), as.Date( end.date ) );

  data = run.query( query, DB = MYSQL_DB_NAME );
  if( !is.empty( data ) ) {
    strike.diffs = abs( data$Strike - info$Strike );
    nearest.strike = data$Strike[ strike.diffs == min( strike.diffs ) ][1];

    closest.db.option.symbol = create.option.symbol( Symbol = info$Symbol, Strike = nearest.strike, 
			Type = info$Type, Expiration.Date = info$Expiration.Date );
  } else { 
    closest.db.option.symbol = Option.Symbol;
  };

  return( closest.db.option.symbol );
};

###########################################################################################
# Routine: get.db.option.ts
###########################################################################################

get.db.option.ts <- function( 
  Symbol, 
  Strike = NA, 
  yearMonth = NA, 
  Expiration.Date = NA, 
  Type = NA, 
  BidAsk = 'Ask', 
  start.date = today() %m-% years(10), 
  end.date = last.biz.date()
) {
  if( is.option.symbol( Symbol ) ) {
    info = parse.option.symbol( Symbol );
    underlier.symbol = info$Symbol;
    Strike = info$Strike;
    Expiration.Date = info$Expiration.Date;
    Type = info$Type;
  } else {
    underlier.symbol = Symbol;
  };

  if( is.na( yearMonth ) & !is.na( Expiration.Date ) ) {
    yearMonth = date2yearMonth( Expiration.Date );
  };

  if( BidAsk == 'Ask' || BidAsk == 'Bid' ) { 
   query = sprintf( "SELECT Date, %s FROM YahooOptions WHERE Symbol = '%s' AND yearMonth = %.0f AND Strike = %.2f AND Type = '%s'", 
				BidAsk, underlier.symbol, yearMonth, Strike, Type );
  } else if( BidAsk == 'Mid' ) {
    query = sprintf( "SELECT Date, Bid, Ask FROM YahooOptions WHERE Symbol = '%s' AND yearMonth = %.0f AND Strike = %.2f AND Type = '%s'", 
				underlier.symbol, yearMonth, Strike, Type );
  } else { 
    stop( 'Unsupported BidAsk type in get.db.option.ts: ', BidAsk );
  };

  data = run.query( query, DB = MYSQL_DB_NAME );

  if( nrow( data ) > 0 ) {
    data = unique( data[ order( data$Date ), ] );
    dates = data$Date;
    values = apply( as.matrix( data[,-1], nrow = nrow( data ) ), 1, mean, na.rm = TRUE );

    option.ts = timeSeries( values, dates );
  } else {
    option.ts = timeSeries() 
  };

  if( !is.empty( option.ts ) ) {
    option.ts = ts.range( option.ts, start.date = start.date, end.date = end.date );
  };

  return( option.ts );
}

###########################################################################################
# Routine: get.rt.option.data
#
# Get Real time option data from database for a given ticker
###########################################################################################

get.rt.option.data <- function( 
  ticker, 
  asOfDate = last.option.date(ticker),
  riskFreeRate = 0.01, 
  yearMonths = NA,
  dividendYield = NA
) {
  query = sprintf( "SELECT * FROM IntradayOptions WHERE OptionSymbol LIKE '%s%%' AND Trade_Time > '%s'
                                        AND Trade_Time < '%s';", ticker, asOfDate - 1, asOfDate + 1 );
  data = run.query( query );

  if( nrow( data ) == 0 ) {
    return( NULL )
  };

  if( is.na( dividendYield ) ) {
    dividendYield = get.dividend.yield( ticker );
  };

  option.info = parse.option.symbols( unique( data$OptionSymbol ) );
  if( !is.na( yearMonths ) ) {
    option.info = option.info[ date2yearMonth( option.info$Expiration.Date ) %in% yearMonths, ];
  };

  full.data = cbind( data, option.info[ match( data$OptionSymbol, option.info[,'Option.Symbol'] ), ] );
  deprecated.symbols = grep( '[1-9]', full.data$Symbol );
  if( !is.empty( deprecated.symbols ) ) {
    full.data = full.data[ -deprecated.symbols, ];
  };

  full.data$nDays = pmax( as.numeric( full.data$Expiration.Date ) - as.numeric( today() ), 0 );
 
  ts = get.rt.data( ticker, start.date = asOfDate, end.date = asOfDate );

  trade.times = sort( unique( full.data$Trade_Time ) )

  ask.vol = numeric( nrow(full.data) ) * NaN;
  bid.vol = numeric( nrow(full.data) ) * NaN;
  spots = numeric( nrow(full.data) ) * NaN;
  ctr = 1;
  for( tt in 1:length( trade.times ) ) {
    block = full.data[ full.data$Trade_Time == trade.times[tt], ];
    spot = interpolate.rt( rownames( ts ), trade.times[tt], as.numeric( ts ) );

    for( i in 1:nrow( block ) ) {
      row = block[i,];
      spots[ctr] = spot;

      type = ifelse( row$Type == 'P', 'put', 'call' ); 
      try( 
      {
        bid.vol[ctr] = AmericanOptionImpliedVolatility( type=type, value = row$Bid, underlying = spot, strike = row$Strike, 
		dividendYield = dividendYield, riskFreeRate = riskFreeRate, maturity = row$nDays/365.25, volatility = .3 )$impliedVol;
      }, silent = TRUE );

      try( 
      {
        ask.vol[ctr] = AmericanOptionImpliedVolatility( type=type, value = row$Ask, underlying = spot, strike = row$Strike, 
		dividendYield = dividendYield, riskFreeRate = riskFreeRate, maturity = row$nDays/365.25, volatility = .3 )$impliedVol;
      }, silent = TRUE );

      ctr = ctr + 1;
    };
  };

  all.data = cbind( full.data, data.frame( AskVol = ask.vol, BidVol = bid.vol, Spot = spots ) )
  all.data = all.data[ !is.na( ask.vol ) & !is.na( bid.vol ), ];
  return( all.data ); 
};

###########################################################################################
# Routine: interpolate.rt
###########################################################################################

interpolate.rt <- function( dates, dt, values ) {
  int = approx( difftime( timeDate(dt), timeDate( dates ) ), values, 0, rule = 2 )$y;
  return( int );
};



###########################################################################################
# Routine: get.last.price
###########################################################################################

get.last.price <- function( symbols ) {
  symbols = unique( symbols );

  symbol_string = paste( symbols, collapse = "', '" );
  query = paste( "SELECT mt.Symbol, mt.Trade_Time, ie.Price FROM ", 
                 "( SELECT Symbol, MAX( Trade_Time ) as Trade_Time FROM IntradayEquity ",
                    sprintf( "WHERE Symbol IN ( '%s' ) GROUP BY Symbol ) mt ", symbol_string ), 
		"LEFT JOIN IntradayEquity ie ON ie.Symbol = mt.Symbol AND ie.Trade_Time = mt.Trade_Time; " );

  data = run.query( query );

  missing = setdiff( symbols, data$Symbol );
  if( length( missing ) > 0 ) {
    data = rbind( data, data.frame( Symbol = missing, Trade_Time = '1900-01-01 16:00:00', Price = NA ) );
  };

  ts = timeSeries( data$Price[1], as.Date( data$Trade_Time[1] ), units = data$Symbol[1] ); 
  if( length( symbols ) > 1 ) {
    for( i in 2:length( symbols ) ) {
      ts = merge( ts, timeSeries( data$Price[i], as.Date( data$Trade_Time[i] ), units = data$Symbol[i] ) );
    };
  };

  return( ts );
};

###########################################################################################
# Routine: read.intraday.data
#
# Retrieve intraday data on a set of Symbols from the SQL Database
###########################################################################################

read.intraday.data = function( 
  symbols = c(), 
  start.time = timeDate(0), 
  end.time = timeDate(), 
  DB = MYSQL_DB_NAME
) {
  query = sprintf( "SELECT * FROM %s.IntradayEquity WHERE Trade_Time >= '%s EST' AND Trade_Time <= '%s EST'", 
		DB, format( start.time, '%Y-%m-%d %H:%M' ), format( end.time, '%Y-%m-%d %H:%M' ) );

  if( length( symbols ) > 0 ) {
    symbol_string = paste( symbols, collapse = "', '" );
    query = sprintf( "%s AND Symbol IN ( '%s' ) ", query, symbol_string );
  };

  data  = run.query( query, DB = MYSQL_DB_NAME );
  return( data );
};


###########################################################################################
# Routine: calc.total.return
###########################################################################################

calc.total.return <- function( 
  ticker, 
  start.date = today() %m-% years(3), 
  end.date   = last.biz.date()
) {
  prices = get.time.series( ticker, start.date = start.date, end.date = end.date );
  div.ts = get.dividends.ts( ticker, start.date = start.date, end.date = end.date );

  if( is.empty( div.ts ) ) {
    return( prices )
  };
  
  colnames( div.ts ) = 'Dividends';
  
  div.yield = div.ts / as.numeric( lag( prices, 1 )[ rownames( div.ts ), ] )

  ts = merge( prices, div.yield );
  ts[ is.na( ts$Dividends ), 2 ] = 0; 
  ts = na.locf( ts );
  
  rtns 	   = returns( ts[, ticker ], method = 'discrete' );
  div.rtns = ts[ -1, "Dividends" ] / as.numeric( ts[ nrow( ts ), ticker ] );

  total.rtns = rtns + div.rtns;
  tr.index   = cumulated( total.rtns, method = 'discrete' );
  tr.index   = merge( timeSeries( 1, as.Date( rownames( ts[1, ] ) ), units = ticker ), tr.index ) * as.numeric( ts[ 1, 1 ] );
 
  return( tr.index );
};

###########################################################################################
# Routine: calc.price.return
###########################################################################################

calc.price.return <- function(
  ticker,
  start.date = today() %m-% years(3),
  end.date = last.biz.date()
) {
  total.returns = get.time.series( ticker, start.date = start.date, end.date = end.date, data.type = 'AdjClose' );
  div.ts = get.dividends.ts( ticker, start.date = start.date, end.date = end.date );

  if( is.empty( div.ts ) ) {
    return( total.returns )
  };

  colnames( div.ts ) = 'Dividends';

  div.yield = div.ts / as.numeric( lag( total.returns, 1 )[ rownames( div.ts ), ] )

  ts = merge( total.returns, div.yield );
  ts[ is.na( ts$Dividends ), 2 ] = 0;
  ts = na.locf( ts );

  rtns = returns( ts[, ticker ], method = 'discrete' );
  div.rtns = ts[ -1, "Dividends" ] / as.numeric( ts[ nrow( ts ), ticker ] );

  price.rtns = rtns - div.rtns;
  pr.index = cumulated( price.rtns, method = 'discrete' );
  pr.index = merge( timeSeries( 1, as.Date( rownames( ts[1, ] ) ), units = ticker ), pr.index ) * as.numeric( ts[ 1, 1 ] );

  return( pr.index );
};


###########################################################################################
# Routine: get.insider.trades
###########################################################################################

get.insider_trades <- function() {
  query = "SELECT * FROM InsiderTransactions WHERE Transaction LIKE 'Purchase' AND Position = 'Officer' 
                        AND Type = 'Direct' AND Trade_Date > '2013-09-01'";

  query = "SELECT Symbol, SUM( Value ) FROM InsiderTransactions WHERE Transaction LIKE 'Purchase' AND Position = 'Officer' 
                        AND Type = 'Direct' AND Trade_Date > '2013-09-01' GROUP BY Symbol";

  sql.data = run.query( query );
};


###########################################################################################
# Routine: get.db.equity.symbols
###########################################################################################

get.db.equity.symbols <- function() {

  # Get list of symbols to update from inputs
  filename.inputs = sprintf( '%s/inputs/symbol_lists/equity_symbol_list.txt', DATA_PATH );
  Symbols.inputs = read.table( filename.inputs, stringsAsFactors = FALSE )[,1];

  # Get list of symbols to update from calculated
  equity.pathname = sprintf( '%s/calculated/symbol_lists/equity_symbol_lists', DATA_PATH );
  calc.equity.lists = system( sprintf( 'find %s/*.*', equity.pathname ), intern = TRUE );
  Symbols.calculated = c();
  for( equity.list in calc.equity.lists ) {
    Symbols.calculated = c( Symbols.calculated, read.table( equity.list, stringsAsFactors = FALSE )[,1] );
  };

  # Combine lists
  Symbols = unique( c( Symbols.inputs, Symbols.calculated ) );
  return( Symbols );
};


