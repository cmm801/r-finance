library( "reshape", quietly = TRUE );

###########################################################################################
# Routine: get.etf.symbols
###########################################################################################

get.etf.symbols <- function() {
  etf.list = get.etf.list();
  etf.symbols = etf.list$Symbol;
  return(etf.symbols);
};

###########################################################################################
# Routine: get.equity.exchange.info
###########################################################################################

get.equity.exchange.info <- function() {
  all.exchange.info = read.csv( EQUITY_EXCHANGE_INFO_CSV, stringsAsFactors = FALSE );
  return( all.exchange.info );
};

###########################################################################################
# Routine: remove.US.exchange.suffix
###########################################################################################

remove.US.exchange.suffix <- function( equity.symbols ) {

  # Get the suffixes for US equity exchanges
  exchange.info = get.equity.exchange.info();
  US.exchange.sfx = exchange.info[ exchange.info$Region == 'USA', 'Suffix' ];

  # Loop through and remove US exchange suffixes
  output.symbols = equity.symbols;
  for( i in 1:length( US.exchange.sfx ) ) {
    regex = sprintf( '\\%s$', US.exchange.sfx[i] ); 
    output.symbols = gsub( regex, '', output.symbols );
  };

  return( output.symbols );
};

###########################################################################################
# Routine: get.equity.exchange.listing.info
###########################################################################################

get.equity.exchange.listing.info <- function() {
  all.exchange.info = get.equity.exchange.info();

  files = list.files( EQUITY_EXCHANGE_LISTS_SIMPLE );
  data = c();
  for( i in 1:length(files) ) {
    exchange.file = files[i];
    filename = sprintf( '%s/%s', EQUITY_EXCHANGE_LISTS_SIMPLE, exchange.file );
    listings = read.delim( filename, header = TRUE, stringsAsFactors = FALSE );

    exchange.name = strsplit( exchange.file, '\\.' )[[1]][1];
    exchange.info = all.exchange.info[ all.exchange.info$Code == exchange.name, ]; 
    if( nrow( exchange.info ) == 1 && nchar( exchange.info$Suffix ) > 0 ) {
      listings$Symbol = paste( listings$Symbol, exchange.info$Suffix, sep = '' );
    };
 
    data = rbind( data, data.frame( listings, Exchange = exchange.name ) );
  };

  # Find the currency for each exchange
  exchange.currency = all.exchange.info[ match( data$Exchange, all.exchange.info$Code ), 'Currency' ];
  data = cbind( data, Currency = exchange.currency );
  data$Currency[ is.na( data$Currency ) ] = '';

  # Some exchanges allow multiple currencies, so for those, we need to find the ccy of each security
  mul.exchanges = unique( data$Exchange[ data$Currency == 'MUL' ] );
  for( exchange.name in mul.exchanges ) {
    suffix = all.exchange.info[ all.exchange.info$Code == exchange.name, 'Suffix' ]

    detailed.info = get.stock.exchange.listings( exchange.name, type = 'Detailed' )
    mul.symbols = gsub( suffix, '', data[ data$Exchange == exchange.name, 'Symbol' ] );
   
    ccy = detailed.info[ match( mul.symbols, gsub( suffix, '', detailed.info$Symbol ) ), 'Currency' ];
    ccy[ is.na( ccy ) ] = '';

    data$Currency[ data$Exchange == exchange.name ] = ccy;
  };
    
  return( data );
};

###########################################################################################
# Routine: get.all.equity.symbols
###########################################################################################

get.all.equity.symbols <- function() {
  etf.symbols = get.etf.symbols();
  exchange.listings = get.equity.exchange.listing.info()

  equity.symbols = unique( c( etf.symbols, exchange.listings$Symbol ) );
  equity.symbols = remove.US.exchange.suffix( equity.symbols );
  return( equity.symbols );
};

###########################################################################################
# Routine: get.index.symbols
###########################################################################################

get.index.symbols <- function() {
  index.info = read.delim( INDEX_CONSTITUENTS_TXT, header = TRUE, stringsAsFactors = FALSE );
  index.symbols = unique( index.info$Index);
  return( index.symbols );
};

###########################################################################################
# Routine: get.index.constituents
###########################################################################################

get.index.constituents <- function( index.symbol = NA ) {
  index.info = read.delim( INDEX_CONSTITUENTS_TXT, header = TRUE, stringsAsFactors = FALSE );
  if( !is.na( index.symbol ) ) {
    index.info = index.info[ index.info$Index == index.symbol, ];
  };

  return( index.info$Symbol );
};

###########################################################################################
# Routine: get.index.constituents
###########################################################################################

get.index.data <- function( 
  index.symbol, 
  code
) {
  index.constituents = get.index.constituents( index.symbol );
  stop('Unsupported');
};

##############################################################################################################
# Routine: get.stock.exchange.listings
##############################################################################################################

get.stock.exchange.listings <- function( 
  exchange.name, 
  type = 'Simple' 
) {
  if( type == 'Simple' ) {    
    filename = sprintf( '%s/%s.txt', EQUITY_EXCHANGE_LISTS_SIMPLE, exchange.name );
    listings = read.delim( filename, header = TRUE, stringsAsFactors = FALSE );
  } else if( type == 'Detailed' ) {
    filename = sprintf( '%s/%s.csv', EQUITY_EXCHANGE_LISTS_DETAILED, exchange.name );
    listings = read.csv( filename, sep="\t", header = TRUE, stringsAsFactors = FALSE );
  } else {
    stop( 'Unsupported type.' );
  };

  return( listings );
};

###########################################################################################
# Routine: get.equity.exchange.suffix
#
# Get the suffix from an array of equity tickers
###########################################################################################

get.equity.exchange.suffix <- function( equity.symbols ) {
  split.symbols = strsplit( equity.symbols, '[.$]' )
  suffix = unlist( lapply( split.symbols, function(x) ifelse( length(x) > 1, x[[length(x)]], '' ) ) )
  return( suffix );
};

###########################################################################################
# Routine: get.equity.exchange.info.from.suffix
#
# Find the exchange for a given ticker from its suffix
###########################################################################################

get.equity.exchange.info.from.suffix <- function( equity.symbols ) {
  sfx = get.equity.exchange.suffix( equity.symbols );
  exchange.listings = get.equity.exchange.listing.info()
  exchange.info = get.equity.exchange.info();

  idx = match( sfx, gsub( '[.]', '', exchange.info$Suffix ) )
  idx[ sfx == '' ] = NA;
  
  col = rep( '', length( equity.symbols ) );      
  output = data.frame( Symbol = equity.symbols, Exchange = col, Currency = col, Suffix = col, Region = col );
  
  output$Symbol = equity.symbols;
  output[ !is.na(idx), c( 'Exchange', 'Currency', 'Suffix', 'Region' ) ] = 
        exchange.info[ idx[!is.na(idx)], c( 'Code', 'Currency', 'Suffix', 'Region' ) ];
 
  return( output );
};

##############################################################################################################
# Routine: get.etf.list
##############################################################################################################

get.etf.list <- function( path = DATA_PATH ) {
  etf.list = read.csv( ETF_LIST_CSV, sep="\t", header = TRUE, stringsAsFactors = FALSE );
  return( etf.list );
};

###########################################################################################
# Routine: get.equity.data.codes
###########################################################################################

get.equity.data.codes <- function( DB = MYSQL_DB_NAME ) {
  query = sprintf( 'SELECT * FROM %s.YahooDataCodes WHERE Include = 1', DB );
  code_info = run.query( query );
  return( code_info );
};

###########################################################################################
# Routine: get.eq.code.from.short.name
###########################################################################################

get.eq.code.from.short.name <- function( short.name ) {
  code_info = get.equity.data.codes();
  code = code_info$Code[ tolower( code_info$ShortName ) == tolower( short.name ) ];
  return( code );
};

###########################################################################################
# Routine: get.equity.data
###########################################################################################

get.equity.data <- function( 
  symbols, 
  code = '', 
  type = '' 
) {
  if( code == '' ) {
    code = get.eq.code.from.short.name( type );
  };

  sym.str = paste( symbols, sep = '', collapse = "','" );

  query = sprintf( paste( c( "SELECT CD.* FROM CompanyData CD Inner Join ( ",
	      "SELECT Symbol, MAX(Date) as Date FROM CompanyData ",
	      "WHERE Symbol in ('%s')  AND Code = '%s' GROUP BY Symbol ) SD ",
           "WHERE CD.Symbol = SD.Symbol AND CD.Date = SD.Date AND CD.Symbol in ( '%s' ) and Code ='%s'" ), collapse = '' ), 
		sym.str, code, sym.str, code  );

  equity.data = run.query( query );
  return( equity.data );
};

###########################################################################################
# Routine: get.all.equity.data
###########################################################################################

get.all.equity.data <- function( 
  symbols, 
  code = '', 
  type = ''
) {
  if( code == '' ) {
    code = get.eq.code.from.short.name( type );
  };

  sym.str = paste( symbols, sep = '', collapse = "','" );

  query = sprintf( "SELECT * FROM CompanyData WHERE Symbol in ('%s') AND Code = '%s'", sym.str, code );
  data = run.query( query );
  return( data );
};

###########################################################################################
# Routine: get.equity.data.ts
###########################################################################################

get.equity.data.ts <- function( 
  symbols, 
  code = '', 
  start.date = today() %m-% years(1), 
  end.date = today(), 
  rm.na = TRUE, 
  type = ''
) {
  if( code == '' ) {
    code = get.eq.code.from.short.name( type );
  };

  data =  get.db.data( symbols, value.name = 'Value', Table = 'CompanyData', Where = sprintf( "Code = '%s'", code ), 
			start = start.date, end = end.date, rm.na = rm.na );
  return( data );
};

###########################################################################################
# Routine: get.dividend.yield.ts
###########################################################################################

get.dividend.yield.ts <- function(
  symbols,
  start.date = today() %m-% years(1),
  end.date = today(),
  rm.na = TRUE
) {
  ts = get.equity.data.ts( symbols, code = 'y', start.date = start.date, end.date = end.date, rm.na = rm.na );

  if( is.empty( ts ) ) {
    ts = timeSeries( rep( 0, length( symbols ) ), end.date, units = symbols );
  };

  while( ncol(ts) < length(symbols) ) {
    ts = nanmerge( ts, ts[,1] * 0, rm.na = rm.na );
  };

  return( ts );
};

###########################################################################################
# Routine: get.stock.split.data
###########################################################################################

get.stock.split.data <- function() {
  stock.split.data = read.csv( STOCK_SPLIT_CSV, sep="\t", header = TRUE, stringsAsFactors = FALSE );
  return( stock.split.data );
};

###########################################################################################
# Routine: get.stock.split.ts
###########################################################################################

get.stock.split.ts <- function(
  symbols,
  start.date = today() %m-% years(10),
  end.date = today()
) {
  # Get all stock split info
  data = get.stock.split.data();

  # Only keep the data for the symbols requested
  data = data[ data$Ticker %in% symbols, ];

  # Only keep splits between the start and end dates
  if( !is.empty( data ) ) {
    data = data[ start.date <= data$Date & data$Date <= end.date, ];
  };
  
  if( is.empty( data ) ) {
    return( timeSeries( matrix( 1, ncol = length(symbols) ), start.date, symbols ) );
  };

  # Calculate the split as the ratio of To : From shares
  data$Split = data$To / data$From;

  # Only keep the Date, Ticker, and Split columns
  data = data[,c( 'Date', 'Ticker', 'Split' ) ];

  # Create the time series by using the 'reshape' library
  df = recast( data, Date ~ Ticker, value = 'Split', id.var = c( 'Ticker', 'Date' ), fun.aggregate = function(x) x[1] )

  # Set all NA's to 1
  df[ is.na( df ) ] = 1;

  # Get a time series object from the data.frame
  ts = timeSeries( df[,-1], df[,1], colnames( df )[-1] );
 
  # Add start date if it is missing
  if( !( as.Date( start.date ) %in% as.Date( rownames(ts) ) ) ) {
    new.row = timeSeries( matrix(1,ncol=ncol(ts)), start.date, colnames(ts) );
    ts = merge( new.row, ts );
  } else {};

  # Insert columns for symbols that had no splits
  missing.symbols = setdiff( symbols, colnames( ts ) );
  for( sym in missing.symbols ) {
    ts = merge.ts( ts, timeSeries( rep( 1, nrow(ts) ), row.names(ts), sym ) );
  };

  # Re-order the columns to be the same as the input array of 'symbols'
  ts = ts[, symbols ];

  return( ts );
};

###########################################################################################
# Routine: get.dividend.yield
###########################################################################################

get.dividend.yield <- function( symbols, na.to.zero = TRUE ) {
  data = get.equity.data( symbols, code = 'y' );

  if( is.empty( data ) ) {
    div.yield = rep( 0, length( symbols ) );
  } else {
    div.yield = data[ match( symbols, data$Symbol ), 'Value' ] / 100;
  };

  if( na.to.zero ) {
    div.yield[ is.na( div.yield ) ] = 0;
  };

  return( div.yield );
};


