
library( lubridate );

source( sprintf( "%s/download.R", DATABASE_PATH ) );

###########################################################################################
# Routine: download.VIX
#
# Download VIX futures data from the CBOE website, and write CSV file
###########################################################################################

download.VIX = function( 
  start.date = as.Date( '2003-12-31' ), 
  end.date   = today() + years(1)
)
{
  futures.id.info <- futures.expiry.symbol();

  data = data.frame();
  i = 0;
  while( !is.na( i ) ) {
    month = 1 + ( ( i - 1 ) %% 12);
    year  = year( start.date ) + floor( ( month( start.date ) + i - 1 ) / 12 );

    label = sprintf("%s%.2d", futures.id.info$Symbol[ month ], year - 2000 )
    url <- sprintf( "http://cfe.cboe.com/Publish/ScheduledTask/MktData/datahouse/CFE_%s_VX.csv", label );

    new.data = data.frame();
    try( { 
      print( url ); 
      new.data <- download.csv( url, sep = ',' );
      # Add this hack to remove warning message that CBOE started adding to CSV files in 7/2013
      if( nrow( new.data ) > 0 && nrow( data ) > 0 ) {
        new.data = new.data[ new.data[,2] != colnames( data )[2],]
        colnames( new.data ) = colnames( data );
      };
    }, silent=TRUE );

    if( nrow( data ) > 0 ) {
      data <- rbind( data, new.data );
    } else {
      data <- new.data;
    };

     i = ifelse( month >= month( end.date ) && year >= year( end.date ), NA, i+1 );
  };

  # Format the date column
  data[, 'Trade_Date' ] = as.Date( data[, 'Trade_Date' ], '%m/%d/%Y' );

  # Format the numeric columns
  numericCols = setdiff( colnames( data ), c( 'Trade_Date', 'Futures' ) );
  for( i in 1:length( numericCols ) ) {
    data[, numericCols[i] ] = as.numeric( data[, numericCols[i] ] );
  };

  # Rescale VIX data before March 26, 2007 by dividing by 10
  affected.dates <- data$Trade_Date < as.Date( "2007-03-26" );
  affected.qtys <- c( "Open", "High", "Low", "Close", "Change", "Settle" );
  data[ affected.dates, affected.qtys ] = data[ affected.dates, affected.qtys ] / 10;
  return( data );
};

###########################################################################################
# Routine: download.csv
#
# This provides another method for importing data from csv files, in case the csv file
# has been corrupted by the presence of additional delimiters at the end of the line.
###########################################################################################

download.csv <- function( filename, sep = ',' ) {
  all.data = read.delim( filename, stringsAsFactors = FALSE, header = FALSE )

  header    = strsplit( all.data[1, ], sep );
  list.data = strsplit( all.data[ 2:dim( all.data )[1], ], sep );

  df = data.frame( matrix( unlist( list.data ), nrow = dim( all.data )[1] - 1, byrow = TRUE ), stringsAsFactors = FALSE );
  names( df ) = gsub( ' ', '_', header[[1]] );
  return( df );
};

###########################################################################################
# Routine: futures.expiry.symbol
#
# Returns mapping between futures month and the symbol used by the CBOE
###########################################################################################

futures.expiry.symbol <- function() {
  futures.months <- c( "F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z" )
  df <- data.frame( month.name = month.name, Symbol = futures.months, month.number = 1:12 )

  return( df );
};

