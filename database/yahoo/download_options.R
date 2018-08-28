
library( XML, quietly = TRUE );
library( lubridate, quietly = TRUE );

source( paste( HOME, "/programming/R/database/download.R", sep = "" ) );

###########################################################################################
# Routine: download.yahoo.option.data
#
# Download option data from Yahoo for all expiries of a given symbol
# Parse the option symbol
###########################################################################################

download.yahoo.option.data <- function( 
  symbol, 
  n.retry = 3, 
  sleep.time = 5
) {
  opt.data = data.frame();

  for( Year in year(today()):(year(today())+3)) {
    for( Month in 1:12 ) {
      print( Year*100 + Month );
      new.data = download.yahoo.option.data.simple( symbol, Year = Year, Month = Month, n.retry = n.retry, sleep.time = sleep.time );
      opt.data = rbind( opt.data, new.data );
    };
  };

  return( opt.data );
};

###########################################################################################
# Routine: download.yahoo.raw.option.data
#
# Download option data from Yahoo for all expiries of a given symbol.
# Don't parse the option symbol - just leave the data as is.
###########################################################################################

download.yahoo.raw.option.data <- function(
  symbol,
  n.retry = 3,
  sleep.time = 5
) {
  opt.data = data.frame();

  for( Year in year(today()):(year(today())+3)) {
    for( Month in 1:12 ) {
      new.data = download.yahoo.raw.option.data.simple( symbol, Year = Year, Month = Month, n.retry = n.retry, sleep.time = sleep.time );
      opt.data = rbind( opt.data, new.data );
    };
  };

  return( opt.data );
};

###########################################################################################
# Routine: download.yahoo.option.data.simple
#
# Download option data from Yahoo for a given ticker, month, and year
###########################################################################################

download.yahoo.option.data.simple <- function(
  ticker,
  Year,
  Month,
  n.retry = 3,
  sleep.time = 5
)
{
  data = download.yahoo.raw.option.data.simple( ticker, Year = Year, Month = Month, n.retry = n.retry, sleep.time = sleep.time );

  if( nrow( data ) == 0 ) {
    return( data );
  };

  option.info = parse.option.symbols( data$OptionSymbol );
  data = cbind( data, option.info[, c( 'Symbol', 'Expiration.Date', 'Type' ) ] ); 

  # Only keep usual expiries, from third Friday and Saturday of the month
  ordinary.expiries = c( get.expiries(), get.expiries()-1 );
  data = data[ data$Expiration.Date %in% ordinary.expiries, ];

  # When there are stock splits, the Option symbol has an extra digit appended to the date, and we ignore these.
  clean.inds = !is.na( data$Strike * data$Ask * data$OpenInt ) & data$Symbol == ticker;
  data = data[ clean.inds, ];

  yearMonth = year( data$Expiration.Date ) * 100 + month( data$Expiration.Date );
  data = cbind( data, yearMonth = yearMonth );

  data = data[, c( 'Symbol', 'Type', 'Strike', 'Ask', 'Bid', 'OpenInt', 'yearMonth' ) ];
  return( data );
};


###########################################################################################
# Routine: get.yahoo.option.url
#
# Get the URL for yahoo options for a given ticker and expiry.  If yearMonth is provided
#  instead of expiration.date, then two url's are returned, corresponding to the third 
#  Friday and third Saturday of the month.
###########################################################################################

get.yahoo.option.url <- function(
  ticker, 
  yearMonth = NA, 
  expiration.date = NA
) {
  if( !is.na( yearMonth ) && !is.na( expiration.date ) ) {
    error( 'Only one of yearMonth or expiration.date should be provided.' );
  } else if( !is.na( expiration.date ) ) {
    expiry = expiration.date;
    count = ( as.numeric( expiry ) - as.numeric( as.Date( '1970-1-1' ) ) ) * 24 * 3600;
    url = sprintf( 'http://finance.yahoo.com/q/op?s=%s&date=%d', ticker, count );
    return(url);
  } else {
    Month = yearMonth %% 100;
    Year = floor( yearMonth / 100 );
    start.date = as.Date( sprintf( '%s-%s-1', Year, Month ) );
    end.date = start.date + months(1) + 1;
    expiry = get.expiries( start.date = start.date - 1, end.date = end.date );
    count = ( as.numeric( expiry ) - as.numeric( as.Date( '1970-1-1' ) ) ) * 24 * 3600;
    fri.url = sprintf( 'http://finance.yahoo.com/q/op?s=%s&date=%d', ticker, count - 24*3600 );
    sat.url = sprintf( 'http://finance.yahoo.com/q/op?s=%s&date=%d', ticker, count );
    return( c( fri.url, sat.url ) );
  }; 
};

###########################################################################################
# Routine: download.yahoo.raw.option.data.simple
#
# Download option data from Yahoo for a given ticker, month, and year
###########################################################################################

download.yahoo.raw.option.data.simple <- function(
  ticker,
  Year,
  Month, 
  n.retry = 3, 
  sleep.time = 5
) {
  yearMonth = Year*100 + Month;
  # Get both the Friday and Saturday expiry URL's
  urls = get.yahoo.option.url( ticker, yearMonth = yearMonth );
  args = list( doc = urls[1], which = c(2,3), stringsAsFactors = F );
  raw.data = run.with.retry( readHTMLTable, args, n.retry = n.retry, sleep.time = sleep.time );
  # If the Friday URL is empty, try the Saturday URL
  if( length( raw.data[[1]] ) == 0 && length( raw.data[[2]] ) == 0 ) {
    args$doc = urls[2];
    raw.data = run.with.retry( readHTMLTable, args, n.retry = n.retry, sleep.time = sleep.time );
  };

  opt.data = rbind( raw.data[[1]], raw.data[[2]] );

  if( is.null( opt.data ) || length( opt.data ) == 0 ) {
    return( data.frame() );
  } else {
    col.names = c( 'Strike', 'OptionSymbol', 'Last', 'Bid', 'Ask', 'Change', 'PctChg', 'Volume', 'OpenInt', 'ImpliedVol' );
    colnames( opt.data ) = col.names;

    for( col in col.names ) {
      if( col %in% c( 'Type', 'OptionSymbol' ) ) {
        next;
      };
    
      opt.data[[col]] = as.numeric( gsub( ",", "", as.character( opt.data[[col]] ) ) );
    };

    opt.data = opt.data[, c( 'OptionSymbol', 'Strike', 'Ask', 'Bid', 'Volume', 'OpenInt' ) ];
    return( opt.data );
  };
};

###########################################################################################
# Routine: parse.option.symbol
#
# Extract Symbol, Strike, Expiration, and Type info from option symbol
###########################################################################################

parse.option.symbol <- function( option.symbol ) {
  chars  = strsplit( option.symbol, '' )[[1]];
  len = length( chars );

  if( len <  16 ) {
    return( NA );
  };

  Strike = as.numeric( paste( chars[ (len-7):len ], collapse = '' ) ) / 1000;
  Type   = chars[ len - 8 ];

  # Find Expiration Date
  Year2 = paste( chars[ (len-14):(len-13) ], collapse = '' );
  Month = paste( chars[ (len-12):(len-11) ], collapse = '' );
  Day   = paste( chars[ (len-10):(len- 9) ], collapse = '' );

  Expiration.Date = sprintf( '20%02d-%s-%s', as.numeric( Year2 ), Month, Day );

  Symbol = paste( chars[ 1:(len-15) ], collapse = '' );

  option.info = list( Symbol = Symbol, Type = Type, Strike = Strike, Expiration.Date = Expiration.Date, Option.Symbol = option.symbol  );
  return( option.info );
};

###########################################################################################
# Routine: parse.option.symbols
#
# Extract Symbol, Strike, Expiration, and Type info from option symbol
###########################################################################################

parse.option.symbols <- function( Option.Symbols ) {
  parsed = list( length( Option.Symbols ) );

  for( k in 1:length( Option.Symbols ) ) {
    Option.Symbol = Option.Symbols[k];
    parsed[[k]] = parse.option.symbol( Option.Symbol );
  }

  df = c();
  for( n in names( parsed[[1]] ) ) {
    df = cbind( df, unlist( lapply( parsed, function(x) x[n] ) ) );
  };

  df = as.data.frame( df, stringsAsFactors = FALSE );
  colnames( df ) = names( parsed[[1]] );
  rownames( df ) = NULL;

  df$Strike = as.numeric( df$Strike );
  df$Expiration.Date = as.Date( df$Expiration.Date );

  return( df );
};

###########################################################################################
# Routine: get.expiries
#
# Returns an array of all (hypothetical and real) Option expiries.  These
# are read in from a .txt file.  The input arguments can be in the form of start/end dates 
# or start/end yearMonths
###########################################################################################

get.expiries <- function(
  start.date = as.Date( '2004-01-01' ),
  end.date   = today() + years(3),
  start.ym   = NA,
  end.ym     = NA
) {

  if( is.na( start.ym ) ) {
    sd = start.date;
  } else {
    sd = yearMonth2date( start.ym, method = 'first' );
  };

  if( is.na( end.ym ) ) {
    ed = end.date;
  } else {
    ed = yearMonth2date( end.ym, method = 'last' );
  };

  filename = sprintf( "%s/calculated/US_Option_expiries.txt", DATA_PATH );

  expiries.char = readLines( filename );
  expiries = as.Date( expiries.char );
  expiries = expiries[ sd <= expiries & expiries <= ed ];
  return( expiries );
};


