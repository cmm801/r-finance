
source( paste( DATABASE_PATH, "/yahoo/generic.R", sep="" ) );
library( 'lubridate', quietly = TRUE );

###########################################################################################
# Routine: get.quotes
# 
# Download Yahoo equity data for a single ticker
###########################################################################################

get.quotes <- function( ticker, 
	      		from=(Sys.Date()-5 * 365),
			to=(Sys.Date()),
			interval="d"
) {
  # put together the URL
  url <- get.yahoo.url( ticker, from = from, to = to, interval = interval );
browser()
  # get the file
  data = data.frame();
  try( {
    tmp <- read.csv( url, stringsAsFactors = FALSE );

    # add a new column with ticker Symbol labels
    if( nrow( tmp ) > 0 ) {
      data = cbind( Symbol=ticker, tmp )
      colnames( data ) = gsub( '\\.', '', colnames( data ) )
    } else {
      data = cbind( data.frame( Symbol = character(0), tmp ) )
    };
  }, silent = TRUE );

  if( nrow( data ) > 1 ) {
    data = data[ order( as.Date( data$Date ) ), ];
  } else {};

  return( data );
}  

###########################################################################################
# Routine: get.yahoo.url
# 
# Get URL where Yahoo data can be obtained
###########################################################################################

get.yahoo.url <- function( ticker,
                        from=(Sys.Date()-5 * 365),
                        to=(Sys.Date()),
                        interval="d"
) {
  #define parts of the URL
  base    <- "http://ichart.finance.yahoo.com/table.csv?"
  Symbol  <- paste("s=", ticker, sep="")

  #months are numbered from 00 to 11, so format the month correctly
  from.month  <- paste( "&a=", month( from ) - 1, sep = '' );
  from.day    <- paste( "&b=", day( from ),       sep = "" );
  from.year   <- paste( "&c=", year( from ),      sep = "" );

  to.month  <- paste( "&d=", month( to ) - 1, sep = '' );
  to.day    <- paste( "&e=", day( to ),       sep = "" );
  to.year   <- paste( "&f=", year( to ),      sep = "" );

  inter <- paste( "&g=", interval, sep="" )
  last  <- "&ignore=.csv"

  # put together the URL
  url <- paste( base, Symbol, from.month, from.day, from.year, to.month, to.day, to.year, inter, last, sep="")

  return(url);
};

###########################################################################################
# Routine: get.stock.split.url
# 
# Get URL where Yahoo data can be obtained
###########################################################################################

get.stock.split.url <- function( ticker,
                        from=(Sys.Date()-5 * 365),
                        to=(Sys.Date())
) {
  base.url <- get.yahoo.url( ticker, from = from, to = to, interval = 'v' );
  stock.split.url = gsub( 'table.csv', 'x', base.url )
  return( stock.split.url );
};

###########################################################################################
# Routine: get.stock.split.info
# 
# Get info on stock splits
###########################################################################################

get.stock.split.info <- function( ticker,
                        from = Sys.Date() - 5 * 365,
                        to = Sys.Date(), 
  			n.retry = 2, 
			sleep.time = 3
) {
  stock.split.url = get.stock.split.url( ticker, from = from, to = to );

  args = list( file = stock.split.url, header = FALSE, stringsAsFactors = FALSE, sep = ',' );
  raw.data = run.with.retry( read.csv, args, n.retry = n.retry, sleep.time = sleep.time, throw.error = FALSE );
  if( !is.null( raw.data ) ) {
    stock.split.rows = which( raw.data[,1] == 'SPLIT' );
    stock.split.data = raw.data[stock.split.rows,];
    if( nrow( stock.split.data ) > 0 ) {
    
      # Parse the split information
      split.col = strsplit( stock.split.data[,3], ':' );
      from.shares = unlist( lapply( split.col, function(x) x[[1]] ) );
      to.shares = unlist( lapply( split.col, function(x) x[[2]] ) );
   
      # Parse data info
      dates = as.numeric( stock.split.data[,2] );
      Y = round( dates / 1e4 );
      M = round( dates / 100 ) %% 100;
      D = dates %% 100;
      parsed.dates = as.Date( sprintf( '%d-%d-%d', Y, M, D ) );

      output.data = data.frame( Ticker = ticker, Date = parsed.dates, From = from.shares, To = to.shares );
      output.data = output.data[ order( output.data$Date ), ];
    } else {
      output.data = data.frame( Ticker = NULL, Date = NULL, From = NULL, To = NULL );
    };
  } else {
    output.data = data.frame( Ticker = NULL, Date = NULL, From = NULL, To = NULL );
  };

  return( output.data );
};

###########################################################################################
# Routine: get.multiple.quotes
# 
# Download Yahoo equity data for multiple tickers
###########################################################################################
get.multiple.quotes <- function( 
  tickers,
  from         = Sys.Date() - 5 * 365, 
  to	       = Sys.Date(),
  interval     = "d"
) {
  quotes = data.frame();  

  for ( tkr in tickers ) { 
    quotes <- rbind( quotes, get.quotes(tkr, from = from, to = to, interval = interval ) )
  };

  return( quotes )
};

###########################################################################################
# Routine: download.yahoo.intraday.equity.data
###########################################################################################

download.yahoo.intraday.equity.data <- function(
  Symbols,
  Table = 'IntradayEquity',
  DB = MYSQL_DB_NAME,
  n.retry = 3, 
  sleep.time = 5, 
  max.batch.size = 200
) {

  quotes = c()
  i = 1;
  while( i <= length( Symbols ) ) {
    i2 = min( length(Symbols), i + max.batch.size - 1 );
    syms = Symbols[i:i2];
    url = sprintf( 'http://finance.yahoo.com/d/quotes.csv?s=%s&f=sl1vd1t1', paste( syms, collapse = '+' ) );

    args = list( file = url, header = FALSE, stringsAsFactors = FALSE );
    new.quotes = run.with.retry( read.csv, args, n.retry = n.retry, sleep.time = sleep.time );
    quotes = rbind( quotes, new.quotes );

    i = i + max.batch.size - 1;
  };

  trade.times = format.yahoo.trade.times( dates = quotes[,4], times = quotes[,5] );

  yahoo.data = data.frame( Symbol = quotes[,1], Trade_Time = trade.times, Price = quotes[,2], Volume = quotes[,3] );
  yahoo.data = yahoo.data[ !is.na( yahoo.data$Trade_Time ) & yahoo.data$Price > 0, ];
  return( yahoo.data );
};

###########################################################################################
# Routine: get.industry.info
###########################################################################################

get.industry.info <- function() {
  url = "http://biz.yahoo.com/p/csv/sum_conameu.csv"
  read.csv(url)
}


###########################################################################################
# Routine: get.equity.currency.info
###########################################################################################

get.equity.currency.info <- function( symbol ) {
  url = sprintf( 'http://www.finance.yahoo.com/q?s=%s', symbol ); 
  lines = readLines( url );
  idx = grep( 'Currency', lines );
  txt = strsplit( lines[idx], '\\s' );
 
  loc = grep( 'Currency', txt[[1]] );
  ccy = substr( txt[[1]][loc+2], 1, 3 );
  return( ccy );
};

