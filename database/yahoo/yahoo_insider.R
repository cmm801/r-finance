
library( XML, quietly = TRUE );
library( TTR, quietly = TRUE );

source( sprintf( '%s/database.R', DATABASE_PATH ) );

###########################################################################################
# Routine: download.yahoo.insider.simple
#
# Get Insider Transactions for a given stock symbol from Yahoo!
###########################################################################################

download.yahoo.insider.simple <- function( symbol ) {
  options(stringsAsFactors = FALSE);
  url = sprintf( "http://finance.yahoo.com/q/it?s=%s+Insider+Transactions", symbol );
  all.data = try( readHTMLTable( url ), silent = TRUE );
  
  if( identical( all.data, 'try-error' ) || is.null( all.data ) || length( all.data ) < 15 || is.null( all.data[[15]] ) ) {
    return( NULL )
  };

  table = all.data[[15]];

  type = table$Type;
  shares = as.numeric( gsub( ',','',table$Shares ) );
  dates = str2date( table$Date );

  # Process Transaction Value information
  raw.Values = as.numeric( gsub( 'NA', 'NA', gsub( ',','',table$Value ) ) );
  
  # Remove footnotes at end of Values
  maxCharSize = unlist( lapply( strsplit( table$Value, ',' ), function(x) max( nchar(x) ) ) );
  if( max( maxCharSize ) > 4 ) {  
    stop( 'Unsupported character format in Yahoo! table' );
  };

  Values = ifelse( maxCharSize == 4, floor( raw.Values / 10 ), raw.Values );

  # Process Insider name
  insiders = table$Insider;
  position.list = c( 'Director', 'Officer', "Beneficial Owner \\(10% or more\\)", "Other" );
  my.positions  = c( 'Director', 'Officer', "Beneficial Owner", "Other" );
  
  positions = character( length( insiders ) );
  for( i in 1:length( position.list ) ) { 
    pos = position.list[i];
    inds = grep( pos, insiders );
    positions[ inds ] = my.positions[i];;
    insiders[inds] = gsub( pos, '', insiders[inds] );
  };

  # Process Transaction
  all.transactions = table$Transaction;
  transactions = character( length( all.transactions ) );
  trans.list = c( 'Acquisition \\(Non Open Market\\)', 'Disposition \\(Non Open Market\\)', 'Sale', 'Automatic Sale', 
				'Option Exercise', 'Statement of Ownership', 'Purchase', "Dividend Reinvestment" );
  my.trans.names = c( 'Acquisition (Non Open Market)', 'Disposition (Non Open Market)', 'Sale', 'Automatic Sale', 
				'Option Exercise', 'Statement of Ownership', 'Purchase', "Dividend Reinvestment" );

  for( i in 1:length( trans.list ) ) {
    trans = trans.list[i];
    inds = grep( trans, all.transactions );
    transactions[inds] = my.trans.names[i];
  }; 

  unclassified = all.transactions[ nchar( transactions ) == 0 ];
  if( length( unclassified ) > 0 ) {
    for( i in 1:length( unclassified ) ) {
      print( unclassified[i] );
    };

    stop( 'Unclassified Transaction types' );
  };

  data = data.frame( Symbol = symbol, Trade_Date = dates, Insider = insiders, Position = positions, 
			Value = Values, Shares = shares, Transaction = transactions, Type = type );
  
  return( data );
};

###########################################################################################
# Routine: download.yahoo.insider
#
# Get Insider Transactions for all stock symbols from Yahoo!
###########################################################################################

download.yahoo.insider <- function() {

  stocks = stockSymbols();
  transactions = c();
  for( i in 1:nrow( stocks ) ) {
    if( i %% 50 == 0 ) {
      print( i );
    };

    symbol = stocks$Symbol[i];
    print( symbol );
    data = download.yahoo.insider.simple( symbol );
    if( nrow( data ) > 0 ) {
      print( symbol );

      if( nrow( transactions ) == 0 ){
        transactions = data;
      } else {
        transactions = rbind( transactions, data );
      }
    };
  };
  
  return( transactions );
};

###########################################################################################
# Routine: get.stock.purchases
#
# Get Insider Transactions for all stock symbols from Yahoo!
###########################################################################################

get.stock.purchases <- function() {

  stocks = stockSymbols( 'NYSE' );

  purchase.data = c();
  for( i in 1:nrow( stocks ) ) {
    if( i %% 50 == 0 ) {
      print( i );
    };

    symbol = stocks$Symbol[i];
    print( symbol );
    data = download.yahoo.insider.simple( symbol );
    purchases = data[ grep( 'Purchase', data$Transaction ), ];
    if( nrow( purchases ) > 0 ) {
      print( symbol );
      mktcap = stocks[ stocks$Symbol == symbol, 'MarketCap' ];
      if( mktcap == 0 ) {
        mktcap = NaN;
      };
  
      purchases = cbind( purchases, data.frame( PercentOfMktCap =  purchases$Value / mktcap, MktCap = mktcap ) );

      if( nrow( purchase.data ) == 0 ){
        purchase.data = purchases;
      } else {
        purchase.data = rbind( purchase.data, purchases );
      }
    }; 
  };
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
# Routine: update.insider.transactions
###########################################################################################
                                
update.insider.transactions <- function(
  Table = 'InsiderTransactions', 
  DB = MYSQL_DB_NAME
) {
  stocks = stockSymbols();

  transactions = c();
  for( i in 1:nrow( stocks ) ) {

    symbol = stocks$Symbol[i];
    new.data = download.yahoo.insider.simple( symbol );
    if( is.null( new.data ) || nrow( new.data ) == 0 ) {
      next;
    };

    current.data.query = sprintf( "SELECT * FROM %s.%s where Symbol = '%s'", DB, Table, symbol );
    current.data = run.query( current.data.query, DB = DB );
    if( nrow( current.data ) == 0 ) {
      data.for.upload = new.data;
    } else {
      # Check for duplicated rows - use 'duplicated' function for characters
      combined.data = rbind( new.data, current.data[,1:(ncol(current.data)-1)] );
      duplicate.rows = duplicated( combined.data, fromLast = TRUE );
 
      new.rows = !duplicate.rows[1:nrow(new.data)];
      data.for.upload = new.data[ new.rows, ];
    };

    if( !is.null( data.for.upload ) && nrow( data.for.upload ) > 0 ) {
      print( symbol );
      if( sum( data.for.upload$Position == '' ) > 0 ) {
        stop( 'Unknown Position type' );
      };

      transactions = cbind( data.for.upload, data.frame( Upload_Date = Sys.Date() ) );
      upload.data( transactions, DB = DB, Table = Table );
    };
  };
};

###########################################################################################
# Routine: str2date
###########################################################################################

str2date <- function( dateStrings ) {
  dates = character( length( dateStrings ) );
  for( i in 1:length( dateStrings ) ) {
    dateString = dateStrings[i];
    L = nchar( dateString );
    year = substr( dateString, L-3, L );
    month = which( month.abb == substr( dateString, 1, 3 ) );
    day = substr( dateString, 5, L-6 );
    dates[i] = sprintf( '%s-%s-%s', year, month, day  );
  };

  dates = as.Date( dates );
  return( dates );
};



