
# Location of R scripts
source( paste( DATABASE_PATH, "/database.R", sep="" ) );
source( paste( DATABASE_PATH, "/download.R", sep="" ) );
source( paste( DATABASE_PATH, "/yahoo/download_equity.R", sep="" ) );

library( RMySQL, quietly = TRUE );

###########################################################################################
# Routine: continuously.save.IntradayEquity
#
# Continuously save intraday data, sleeping in between runs
###########################################################################################

continuously.save.IntradayEquity <- function( 
  Table = 'IntradayEquity', 
  DB = MYSQL_DB_NAME,
  n.retry = Inf, 
  sleep.time = 5, 
  sleep.seconds = 60
) {
  while( TRUE ) {
    try( update.intraday.equity.data( Table = Table, DB = DB, n.retry = n.retry, sleep.time = sleep.time ) );
    # Sleep for 60s between attempts
    Sys.sleep( sleep.seconds )
  };
};

###########################################################################################
# Routine: update.EquityData
#
# Download Yahoo Equity data
###########################################################################################

update.EquityData <- function(
  Table = 'EquityData', 
  DB = MYSQL_DB_NAME
) {
  NewData = prepare.yahoo.equity.data( Table = Table, DB = DB );
  upload.data( NewData, Table = Table, DB = DB );
  nrows.added = nrow( NewData );
  return( nrows.added );
};

###########################################################################################
# Routine: prepare.yahoo.equity.data 
#
# Download Yahoo Equity data
###########################################################################################

prepare.yahoo.equity.data <- function(
  Table = "EquityData",
  DB    = MYSQL_DB_NAME
) {
  # Get list of symbols to update
  Symbols = get.equity.symbols();

  # Get last update dates for Symbols already in the database
  db.table = paste( DB, Table, sep = '.' );
  query = paste( c( "SELECT dt.Symbol, dt.Date, eq.AdjClose FROM( SELECT Symbol, MAX( Date ) as Date FROM ", db.table,  
    " GROUP BY Symbol ) dt LEFT JOIN ", db.table, " eq ON dt.Symbol = eq.Symbol AND dt.Date = eq.Date;" ), collapse = '' )

  db.symbol.info = run.query( query, DB = DB );
  existing.symbols = db.symbol.info$Symbol;

  # For symbols not already in the database, append them to the data frame
  new.syms = Symbols[ is.na( match( Symbols, existing.symbols ) ) ]
  firstDate = '1950-01-01';
  if( length( new.syms ) ) {
    symbol.info = rbind( db.symbol.info, data.frame( Symbol = new.syms, Date = firstDate, AdjClose = NaN ) );
  } else {
    symbol.info = db.symbol.info;  
  };

 # Get data for all symbols, and combine them into data frame
  NewData = data.frame();
  for( i in 1:nrow( symbol.info ) ) {
    LastDate = as.Date( symbol.info[ i, "Date" ] );
    symbol  = symbol.info[ i, "Symbol" ];

    # Get new data from Yahoo!
    quotes = get.quotes( symbol, from = LastDate );
   
    # Compare the last Adjusted Close value in the Database with what we just got from Yahoo!
    LastAdjClose  = symbol.info[ i, "AdjClose" ];
    CurrAdjClose= quotes[quotes$Date == LastDate,'AdjClose'];
    
    # If the Adjusted Close agrees in the old and new data, then upload the new data
    if( nrow( quotes ) > 1 ) { 
       if( !is.nan( LastAdjClose ) && abs( LastAdjClose - CurrAdjClose ) < 1e-4 ) { 
         NewData = rbind( NewData, quotes[ quotes$Date != LastDate, ] );
       } else {
         # Otherwise, download the complete time series from Yahoo and upload everything
         print( sprintf( 'Downloading new equity data set for Symbol %s', symbol ) );
    
         # First, delete the existing data
         query = sprintf( "DELETE FROM %s WHERE Symbol = '%s'", db.table, symbol );
         run.query( query, DB = DB );

         # Then get all data from Yahoo!
         quotes = get.quotes( symbol, from = firstDate );
         NewData = rbind( NewData, quotes );
       };
    };
  }

  return( NewData );
};

###########################################################################################
# Routine: upload.equity.currency.denoms
###########################################################################################

upload.equity.currency.denoms <- function(
  n.retry = 3,
  sleep.time = 0.1
) {
  equity.symbols = unique( c( get.equity.symbols(), get.intraday.equity.symbols() ) );

  currencies = character( length( equity.symbols ) );
  for( i in 1:length( equity.symbols ) ) {
    symbol = equity.symbols[i]; 
    print( symbol );
    ccy = run.with.retry( get.equity.currency.info, list( symbol = symbol ), n.retry = n.retry, 
						sleep.time = sleep.time, throw.error = FALSE );
    if( !is.null(ccy) ) {
      currencies[i] = toupper(ccy);
    };
  };

  output = data.frame( Symbol = equity.symbols, Currency = currencies );
  write.table( output, file = EQUITY_CCY_DENOM_TXT, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE )
};

###########################################################################################
# Routine: get.equity.symbols
###########################################################################################

get.equity.symbols <- function() {

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

###########################################################################################
# Routine: get.intraday.equity.symbols
###########################################################################################

get.intraday.equity.symbols <- function() {

  # Get list of symbols to update from inputs
  filename.inputs = sprintf( '%s/inputs/symbol_lists/intraday_equity_symbol_list.txt', DATA_PATH );
  Symbols.inputs = read.table( filename.inputs, stringsAsFactors = FALSE )[,1];

  # Get list of symbols to update from calculated
  filename.calculated = sprintf( '%s/calculated/symbol_lists/intraday_equity_symbol_list.txt', DATA_PATH );
  Symbols.calculated = read.table( filename.calculated, stringsAsFactors = FALSE )[,1];

  # Combine lists
  Symbols = unique( c( Symbols.inputs, Symbols.calculated ) );
  return( Symbols );
};

###########################################################################################
# Routine: update.dividends
###########################################################################################

update.dividends <- function() {
  Symbols = get.equity.symbols();

  dividends = c();
  for( symbol in Symbols ) {
    dividends = rbind( dividends, get.quotes( symbol, from = today()-years(100), to = today(), interval = 'v' ) );
  };

  write.table( dividends, file = DIVIDENDS_CSV, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE )
  nrows.added = nrow( dividends );
  return( nrows.added );
};

###########################################################################################
# Routine: update.stock.splits
###########################################################################################

update.stock.splits <- function() {
  Symbols = get.equity.symbols();

  stock.splits = c();
  for( symbol in Symbols ) {
    stock.splits = rbind( stock.splits, get.stock.split.info( symbol, from = today()-years(100), to = today() ) );
  };

  write.table( stock.splits, file = STOCK_SPLIT_CSV, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE )
  nrows.added = nrow( stock.splits );
  return( nrows.added );
};

###########################################################################################
# Routine: update.intraday.equity.data
###########################################################################################

update.intraday.equity.data <- function(
  Table = 'IntradayEquity', 
  DB = MYSQL_DB_NAME,
  n.retry = 3, 
  sleep.time = 5, 
  max.batch.size = 200
) {
  Symbols = get.intraday.equity.symbols();

  yahoo.data = download.yahoo.intraday.equity.data( Symbols, n.retry = n.retry, sleep.time = sleep.time, 
						max.batch.size = max.batch.size );

  query = sprintf( "SELECT Symbol, MAX( TRADE_TIME ) FROM %s.%s GROUP BY Symbol", DB, Table );
  db.symbol.info = run.query( query, DB = DB );
  if( nrow( db.symbol.info ) > 0 ) {
    db.trade.times = as.POSIXlt( db.symbol.info[ match( yahoo.data$Symbol, db.symbol.info$Symbol ),2], tz = 'EST' );
    new.symbols = !( yahoo.data$Symbol %in% db.symbol.info$Symbol );
    new.times = yahoo.data$Trade_Time > db.trade.times;
    new.data = yahoo.data[ new.symbols | new.times, ];
  } else {
    new.data = yahoo.data;
  };

  upload.data( new.data, DB = DB, Table = Table );
};


