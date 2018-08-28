
source( paste( DATABASE_PATH, "/database.R", sep="" ) );
source( paste( DATABASE_PATH, "/yahoo/generic.R", sep="" ) );
source( paste( DATABASE_PATH, "/yahoo/download_options.R", sep="" ) );

library( RMySQL, quietly = TRUE );

###########################################################################################
# Routine: continuously.save.intraday.option.data
#
# Continuously save intraday data, sleeping in between runs
###########################################################################################

continuously.save.intraday.option.data <- function( 
  Table = 'IntradayOptions', 
  DB = MYSQL_DB_NAME,
  n.retry = Inf, 
  sleep.time = 5
) {
  Symbols = get.intraday.option.symbols();

  while( TRUE ) {
    for( symbol in Symbols ) {
      print( sprintf( 'Getting option data for %s', symbol ) );
      try( update.symbol.IntradayOptions( symbol, Table = Table, DB = DB, n.retry = n.retry, sleep.time = sleep.time ) );
    };
  };
};

###########################################################################################
# Routine: update.symbol.YahooOptions
#
# Download Yahoo Options data
###########################################################################################

update.symbol.YahooOptions <- function( 
  symbol, 
  Table = 'YahooOptions',
  DB = MYSQL_DB_NAME,
  n.retry = 3, 
  sleep.time = 5
) {
  query  = sprintf( "SELECT Symbol, MAX( Date ) as Date FROM %s.%s WHERE Symbol = '%s' GROUP BY Symbol", DB, Table, symbol );
  result = run.query( query, DB = DB );

  # Find last date on which the database was updated
  if( nrow( result ) == 1 ) {
    last.update.date = result$Date;
  } else {
    last.update.date = today() - years(1000);
  };

  # Find the last trade date for which Yahoo data is available
  last.trade.date = get.last.yahoo.trade.date( symbol );

  # If the last trade and last date in the database are the same, then data has already been updated
  if( last.trade.date <= last.update.date ) {
    print( paste( "Option data has already been updated for", symbol ) );
    return()
  };

  # Download option data from Yahoo
  opt.data = download.yahoo.option.data( symbol, n.retry = n.retry, sleep.time = sleep.time );

  # Upload data to the MySQL database
  nrows.added = 0;
  if( nrow( opt.data ) != 0 ) {
    # Add the last update date to the data frame, and then upload to DB
    NewData = cbind( opt.data, Date = last.trade.date );
    upload.data( NewData, Table = Table, DB = DB );
    nrows.added = nrow(NewData);
  };

  return(nrows.added);
};

###########################################################################################
# Routine: update.symbol.IntradayOptions
###########################################################################################

update.symbol.IntradayOptions <- function( 
  symbol, 
  Table = 'IntradayOptions',
  DB = MYSQL_DB_NAME,
  n.retry = 3,
  sleep.time = 5
) {
  # Download option data from Yahoo, and record the time before and after function is called
  start.time = get.last.yahoo.trade.time( symbol );
  new.data = download.yahoo.raw.option.data( symbol, n.retry = n.retry, sleep.time = sleep.time );
  end.time = get.last.yahoo.trade.time( symbol );

  # Calculate trade.time, as the mean of the start and end times
  elapsed.time = difftime(  end.time, start.time, units = 's' ); 
  trade.time = start.time + elapsed.time/2;

  # Get information already in the database
  query = paste( c( "SELECT t2.* FROM (SELECT OptionSymbol, MAX( Trade_Time ) as Trade_Time ", 
	sprintf( "FROM %s.%s WHERE OptionSymbol LIKE '%s%%' GROUP BY OptionSymbol ) t1 ", DB, Table, symbol ), 
        sprintf( "LEFT JOIN %s.%s t2 ON t1.OptionSymbol = t2.OptionSymbol AND t1.Trade_Time = t2.Trade_Time;", DB, Table ) ), 
														collapse = '' );
  db.data = run.query( query, DB = DB );
  if( nrow( db.data ) > 0 ) {
    matched.db.data = db.data[ match( new.data$OptionSymbol, db.data$OptionSymbol ), ];

    # Only keep option data that has changed since the last update
    rows.to.append = !is.na( new.data$Ask * new.data$Volume * new.data$OpenInt );
    rows.to.append = rows.to.append & ( ( is.na( matched.db.data$Ask )     | ( new.data$Ask != matched.db.data$Ask ) )
       			            | ( is.na( matched.db.data$Volume )  | ( new.data$Volume != matched.db.data$Volume ) )
			            | ( is.na( matched.db.data$OpenInt ) | ( new.data$OpenInt != matched.db.data$OpenInt ) ) );
    data.to.upload = new.data[ rows.to.append,];
  } else { 
    data.to.upload = new.data;
  };

  # Upload data to the MySQL database
  if( nrow( data.to.upload ) != 0 ) {
    # Add the last trade time to the data frame, and then upload to DB

    data.to.upload = cbind( new.data, data.frame( Trade_Time = trade.time ) );
    data.to.upload = data.to.upload[ , c( 'OptionSymbol', 'Trade_Time', 'Ask', 'Bid', 'Volume', 'OpenInt' ) ]; 

    upload.data( data.to.upload, Table = Table, DB = DB );
  };
 
  return( nrow( data.to.upload ) );
};

###########################################################################################
# Routine: get.option.symbols
###########################################################################################

get.option.symbols <- function() {

  # Get list of symbols to update from inputs
  filename.inputs = sprintf( '%s/inputs/symbol_lists/option_symbol_list.txt', DATA_PATH );
  Symbols.inputs = read.table( filename.inputs, stringsAsFactors = FALSE )[,1];

  # Get list of symbols to update from calculated
  filename.calculated = sprintf( '%s/calculated/symbol_lists/option_symbol_list.txt', DATA_PATH );
  Symbols.calculated = read.table( filename.calculated, stringsAsFactors = FALSE )[,1];

  # Combine lists
  Symbols = unique( c( Symbols.inputs, Symbols.calculated ) );

  return( Symbols );
};

###########################################################################################
# Routine: get.intraday.option.symbols
###########################################################################################

get.intraday.option.symbols <- function() {

  # Get list of symbols to update from inputs
  filename.inputs = sprintf( '%s/inputs/symbol_lists/intraday_option_symbol_list.txt', DATA_PATH );
  Symbols.inputs = read.table( filename.inputs, stringsAsFactors = FALSE )[,1];
  
  # Get list of symbols to update from calculated
  filename.calculated = sprintf( '%s/calculated/symbol_lists/intraday_option_symbol_list.txt', DATA_PATH );
  Symbols.calculated = read.table( filename.calculated, stringsAsFactors = FALSE )[,1];
  
  # Combine lists
  Symbols = unique( c( Symbols.inputs, Symbols.calculated ) );
  
  return( Symbols );
};



