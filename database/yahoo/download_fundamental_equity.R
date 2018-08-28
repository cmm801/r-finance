
library( 'XML', quietly = TRUE );
library( 'TTR', quietly = TRUE );
library( 'quantmod', quietly = TRUE );

options(stringsAsFactors = FALSE);

source( sprintf( "%s/download.R", DATABASE_PATH ) );
source( sprintf( "%s/database.R", DATABASE_PATH ) );
source( sprintf( "%s/indices/indices.R", DATABASE_PATH ) );

##############################################################################################################
# Routine: download.etf.list
##############################################################################################################

download.etf.list <- function() {
  url = 'http://www.nasdaq.com/investing/etfs/etf-finder-results.aspx?download=Yes'
  etf.list = read.csv( url, header = T, stringsAsFactors = F );
  return( etf.list );
};

##############################################################################################################
# Routine: save.etf.list
##############################################################################################################

save.etf.list <- function( path = DATA_PATH ) {
  etf.list = download.etf.list();
  filename = ETF_LIST_CSV;
  write.table( etf.list, file = filename, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE );  
  print( sprintf( 'Saved %d symbols to %s', nrow( etf.list ), filename ) );
  nrows.added = nrow(etf.list);
  return(nrows.added);
};

##############################################################################################################
# Routine: get.etf.list
##############################################################################################################

get.etf.list <- function( path = DATA_PATH ) {
  filename = ETF_LIST_CSV;
  etf.list = read.csv( filename, sep="\t", header = TRUE, stringsAsFactors = FALSE );
  return( etf.list );
};

##############################################################################################################
# Routine: save.US.stock.lists
##############################################################################################################

save.US.stock.lists <- function() {

  # Get US stock info
  US.stock.lists = stockSymbols();

  # Add a currency column to the US stock data
  US.stock.lists = cbind( US.stock.lists, Currency = rep( 'USD', nrow( US.stock.lists ) ) );

  # Create different data frames for each separate exchange list
  AMEX.stock.list = US.stock.lists[ US.stock.lists$Exchange == 'AMEX', ];
  NYSE.stock.list = US.stock.lists[ US.stock.lists$Exchange == 'NYSE', ];
  NASDAQ.stock.list = US.stock.lists[ US.stock.lists$Exchange == 'NASDAQ', ];

  # Save the data for AMEX to a csv file
  AMEX.filename =  sprintf( '%s/%s.csv', EQUITY_EXCHANGE_LISTS_DETAILED, 'AMEX' );
  write.table( AMEX.stock.list, file = AMEX.filename, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE );
  print( sprintf( 'Saved %d symbols to %s', nrow( AMEX.stock.list ), AMEX.filename ) );

  # Save the data for NYSE to a csv file
  NYSE.filename =  sprintf( '%s/%s.csv', EQUITY_EXCHANGE_LISTS_DETAILED, 'NYSE' );
  write.table( NYSE.stock.list, file = NYSE.filename, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE );
  print( sprintf( 'Saved %d symbols to %s', nrow( NYSE.stock.list ), NYSE.filename ) );

  # Save the data for NASDAQ to a csv file
  NASDAQ.filename =  sprintf( '%s/%s.csv', EQUITY_EXCHANGE_LISTS_DETAILED, 'NASDAQ' );
  write.table( NASDAQ.stock.list, file = NASDAQ.filename, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE );
  print( sprintf( 'Saved %d symbols to %s', nrow( NASDAQ.stock.list ), NASDAQ.filename ) );

  nrows.added = nrow(US.stock.lists);
  return(nrows.added);
};

##############################################################################################################
# Routine: get.US.stock.listings
##############################################################################################################

get.US.stock.listings <- function() {

  # Get filenames for US exchange listings
  AMEX.filename =  sprintf( '%s/%s.csv', EQUITY_EXCHANGE_LISTS_DETAILED, 'AMEX' );
  NYSE.filename =  sprintf( '%s/%s.csv', EQUITY_EXCHANGE_LISTS_DETAILED, 'NYSE' );
  NASDAQ.filename =  sprintf( '%s/%s.csv', EQUITY_EXCHANGE_LISTS_DETAILED, 'NASDAQ' );

  # Get the stock lists
  AMEX.stock.list = read.csv( AMEX.filename, sep="\t", header = TRUE, stringsAsFactors = FALSE );
  NYSE.stock.list = read.csv( NYSE.filename, sep="\t", header = TRUE, stringsAsFactors = FALSE );
  NASDAQ.stock.list = read.csv( NASDAQ.filename, sep="\t", header = TRUE, stringsAsFactors = FALSE );

  # Combine the stock lists
  stock.lists = rbind( rbind( AMEX.stock.list, NYSE.stock.list ), NASDAQ.stock.list );
  return( stock.lists );
};

###########################################################################################
# Routine: download.all.equity.data
###########################################################################################

download.all.equity.data <- function( 
  n.retry = 3,
  sleep.time = 3, 
  batch.size = 200
) {
  stock.info = get.US.stock.listings();
  etf.info   = get.etf.list();
  US.stock.symbols = c( stock.info$Symbol, etf.info$Symbol );
  
  # Exclude symbols with non alphanumeric characters from NYSE, NASDAQ and AMEX exchanges
  US.stock.symbols = US.stock.symbols[ regexpr( "\\W|_", US.stock.symbols ) == -1 ];

  # Get all stocks appearing in major indices  
  index.info = get.index.constituents();
  index.constituents = unique( index.info$Symbol );

  # Combine US stocks with all index constituents
  all.symbols = sort( unique( c( US.stock.symbols, index.constituents ) ) );
  
  # Split symbols into groups of 'batch.size', before getting data from Yahoo
  intervals = seq( from = 1, to = length( all.symbols ), by = batch.size );

  for( i in intervals ) {
    symbols = all.symbols[i:min(length(all.symbols),(i+batch.size-1))];
    print( Sys.time() );
    print( sprintf( 'Downloading data for %s-%s', symbols[1], symbols[length(symbols)] ) );
    new.data = download.equity.data( symbols, n.retry = n.retry, sleep.time = sleep.time );
    if( i == 1 ) {
      data = new.data;
    } else {
      data = rbind( data, new.data );
    };
  };

  return( data );
};    

###########################################################################################
# Routine: download.equity.industry.info
###########################################################################################

download.equity.industry.info <- function() {
  
  data = c();
  for( i in 1:1000 ) {
    if( i %% 20 == 0 ) {
      print(i);
    };

    url = sprintf( 'http://biz.yahoo.com/p/%dconameu.html', i );
    raw.data = try( readHTMLTable( url ), silent = TRUE );
    if( class( raw.data ) == 'try-error' ) {
      next;
    };

    raw.table = raw.data[[5]];
    if( raw.table[3,1] != 'Companies' ) {
      next;
    };

    # Get the Sector and Industry classifications from the first two rows of the table
    raw.table[,1] = gsub( '\n', ' ', raw.table[,1] );
    sector.name = gsub( 'Sector: ', '', raw.table[1,1] );
    industry.name = gsub( ' \\(More Info\\)', '', gsub( 'Industry: ', '', raw.table[2,1] ) );


    # If there is no company data, then continue to the next Industry group
    if( nrow( raw.table ) <= 3 ) {
      next;
    };

    # Parse descriptions to extract Name and Symbol
    descs = raw.table[-(1:3),1];
    eqty.names = character();
    symbols = character();
    # Find location of parentheses, which contain the Symbol
    for( k in 1:length( descs ) ) {
      dsc = descs[k];
      loc = tail( gregexpr( '\\(', dsc)[[1]], 1 ); 
      eqty.names[k] = substr(dsc, 1, loc-2 );
      symbols[k] = substr( dsc, loc+1, nchar(dsc)-1 );
    };

    new.data = data.frame( Sector = sector.name, Industry = industry.name, Name = eqty.names, Symbol = symbols );

    # Remove duplicates
    new.data = new.data[ !duplicated( new.data ), ];

    data = rbind( data, new.data );
  };

  dup.data = data[ data$Symbol %in% data$Symbol[ duplicated( data$Symbol ) ], ];
  output.data = data[ !( data$Symbol %in% data$Symbol[ duplicated( data$Symbol ) ] ), ];
 
  add.data = c();
  rem.data = c(); 
  symbols = unique( dup.data$Symbol );
  for( i in 1:length( symbols ) ) {
    rows = dup.data[ dup.data$Symbol == symbols[i], ];
    if( length( unique( rows$Industry ) ) == 1 && length( unique( rows$Sector ) ) == 1 ) {
      add.data = rbind( add.data, rows[1,] );
    } else {
      rem.data = rbind( rem.data, rows );
    };
  };

  tol = 1e-2;

  missing.symbols = c();
  yahoo.mktcaps = getQuote( unique( rem.data$Symbol ), what = yahooQF( c( 'Symbol', 'Market Capitalization' ) ) );
  symbols = unique( rem.data$Symbol );
  for( i in 1:length( symbols ) ) {
    rows = rem.data[ rem.data$Symbol == symbols[i], ];
    mktcaps = unlist( lapply( rows$MarketCap, function(x) string2num(x) ) ) 
    MC = string2num( yahoo.mktcaps[ yahoo.mktcaps$Symbol == symbols[i], 'Market Capitalization' ] );
    found = rows[ abs( -1 + mktcaps / MC ) < tol, ];
    if( nrow( found ) > 0 ) {
       add.data = rbind( add.data, found[1,] );
    } else {
      missing.symbols = c( missing.symbols, symbols[i] );
    };
  }; 

  print( 'Conflicting information for some symbols: ' );
  print( missing.symbols );

  output.data = rbind( output.data, add.data );

  return( output.data );
}


###########################################################################################
# Routine: download.equity.data
###########################################################################################

download.equity.data <- function( 
  symbols,
  n.retry = 3,
  sleep.time = 3,
  DB = MYSQL_DB_NAME
) {
  query = sprintf( 'SELECT * FROM %s.YahooDataCodes WHERE Include = 1', DB );
  code.info = run.query( query, DB = DB );
  codes = code.info$Code;

  symbol.string =  paste( symbols, sep = '', collapse = '+' );
  code.string =  paste( codes, sep = '', collapse = '+' );
  date.url = sprintf( "http://finance.yahoo.com/d/quotes.csv?s=%s&f=d1s", symbol.string );
  args = list( file = date.url, sep = ',', header = FALSE, stringsAsFactors = FALSE );
  update.dates = run.with.retry( read.csv, args, n.retry = n.retry, sleep.time = sleep.time );

  date.info = data.frame( Symbol = update.dates[,2], Date = update.dates[,1] );
  date.info = date.info[ date.info$Date != 'N/A',];
  date.info$Date = as.Date( date.info$Date, '%m/%d/%Y' );

  data = NULL;
  for( c in 1:length( codes ) ) {
    code = gsub( ' ', '', codes[c] );

    url = sprintf( "http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s", paste( symbols, sep = '', collapse = '+' ), code );
    args = list( file = url, sep = '\t', header = FALSE, stringsAsFactors = FALSE );
    tmp = run.with.retry( read.csv, args, n.retry = n.retry, sleep.time = sleep.time );
    tmp = gsub( ',','',tmp[,1] );
  
    if( 'numeric' == code.info$Class[c] ) {
      tmp = string2num(tmp);
    } else if( 'Date' == code.info$Class[c] ) {
      tmp = as.Date(tmp);
    } else {
      stop( sprintf( 'Unsupported class: %s', code.info$Class[c] ) );
    };

    new.data = data.frame( Symbol = symbols, Value = as.numeric( tmp ), Code = code ); 
    if( is.null( data ) ) {
      data = new.data;
    } else { 
      data = rbind( data, new.data );
    };
  };

  data = data[!is.na(data$Value),];
  data = cbind( data, Date = date.info[ match( data$Symbol, date.info$Symbol ), 'Date' ] );
  data = data[!is.na(data$Date),];

  return( data );
};

