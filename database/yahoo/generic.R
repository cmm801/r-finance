
library( 'lubridate', quietly = TRUE );

###########################################################################################
# Routine: get.last.yahoo.trade.time
#
# Get last trade time for symbols, in POSIXlt
###########################################################################################

get.last.yahoo.trade.time <- function( symbols ) {
  url = sprintf( 'http://finance.yahoo.com/d/quotes.csv?s=%s&f=sd1t1', paste( symbols, collapse = '+' ) );
  data = read.csv(url,header=F, stringsAsFactors = F );

  if( nrow( data ) > 0 ) {
    trade.times = format.yahoo.trade.times( dates = data[,2], times = data[,3] );
  } else { 
    trade.times = data.frame();
  };

  trade.times = trade.times[ match( symbols, data[,1] ) ];
  return( trade.times );
};

###########################################################################################
# Routine: format.yahoo.trade.times
#
# Yahoo's real-time data has time stamps in the afternoon that are 04:00 instead of 16:00.
# This function will correct these dates.
###########################################################################################

format.yahoo.trade.times <- function( dates, times ) {
  dates = as.Date( dates, '%m/%d/%Y');
  is.pm = grep( 'pm', times );
  split.times = strsplit( gsub( 'am', '', gsub( 'pm', '', times ) ), ':' );
  minutes = as.numeric( unlist( lapply( split.times, function(x) x[[2]] )) );
  hours = as.numeric( unlist( lapply( split.times, function(x) x[[1]] )) ) %% 12;
  hours[is.pm] = hours[is.pm] + 12;

  posix.dates = sprintf( '%s %.02d:%.02d EST', dates, hours, minutes );
  posix.dates = as.POSIXlt( posix.dates, tz = 'EST' );
  return( posix.dates );
};

###########################################################################################
# Routine: get.last.yahoo.trade.date
###########################################################################################

get.last.yahoo.trade.date <- function( symbols ) {
  url = sprintf( 'http://finance.yahoo.com/d/quotes.csv?s=%s&f=d1', paste( symbols, collapse = '+' ) );
  date.info = read.csv( url, header = F, stringsAsFactors= F )
  last.trade.date = as.Date( date.info[[1]], '%m/%d/%Y' );
  return( last.trade.date );
};


