
# library( zoo, quietly = TRUE );
#library( FredR, quietly = TRUE );
# library( timeSeries, quietly = TRUE );
library( lubridate, quietly = TRUE );
library( quantmod, quietly = TRUE );


###########################################################################################
# Routine: get.fred.api.key
###########################################################################################

get.fred.api.key <- function() {
  api.key = 'd7fdb9db2be7b781b6840ba33d08e93d';
  return( api.key );
};

###########################################################################################
# Routine: get.fred.api.key
###########################################################################################

get.fred.connection <- function() {
  api.key = get.fred.api.key();
  fred <- FredR(api.key);
  return( fred );
};

###########################################################################################
# Routine: download.fed.data
#
# Download data from FRED using fredR package
###########################################################################################

download.fed.data <- function( symbol ) {

  result = try( {
    getSymbols( symbol, src = 'FRED', method = 'wget' )
    eval( parse( text = paste( 'new.ts <- ', symbol ) ) );
    # fred = get.fred.connection();
    # new.ts = fred$series.observations(symbol);
  }, silent = TRUE );

  if( identical( class( result ), 'try-error' ) ) {
    return( NULL );
  };
    
  #dates = as.Date( new.ts$date );
  dates = index( new.ts );

  # Move months to last day of week for Monthly & Quarterly data.
  # Otherwise, monthly data from FRED appears (incorrectly) as being from the first date of the month
  freq = round( 365 / mean( diff( as.numeric( dates ) ) ) );
  if( freq <= 12 ) {
    dates = dates + months( 1 ) - days( 1 );
  };

  #new.data = data.frame( Symbol = symbol, Date = dates, Value = as.numeric( new.ts$value ) );
  new.data = data.frame( Symbol = symbol, Date = dates, Value = as.numeric( new.ts ) );

  # Remove NULL values
  new.data = new.data[ !is.null( new.data$Value ), ];
  return( new.data );
};

