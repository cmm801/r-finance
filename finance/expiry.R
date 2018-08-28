library( lubridate );

###########################################################################################
# Routine: futures.expiry.symbol
#
# Returns mapping between futures month and the symbol used by the CBOE
###########################################################################################

futures.expiry.symbol <- function( month = -1 ) {
  futures.months <- c( "F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z" )
  df <- data.frame( month.name = month.name, Symbol = futures.months, month.number = 1:12 )

  return( df );
};

###########################################################################################
# Routine: get.expiry.filename
#
# Get the full path name for the text files that contain option expiry dates
###########################################################################################

get.expiry.filename <- function( name ) {
  if( name == 'VIX' ) {
    filename = VIX_EXPIRIES_TXT;
  } else if( name == 'US Options' ) {
    filename = US_OPTION_EXPIRIES_TXT;
  } else {
    stop( sprintf( 'No known filename for argument %s', name ) );
  };

  return( filename );
};

###########################################################################################
# Routine: get.expiries
#
# Returns an array of all (hypothetical and real) Option or Futures expiries.  These
# are read in from a .txt file.  The input arguments can be in the form of start/end dates 
# or start/end yearMonths
###########################################################################################

get.expiries <- function(
  name, 
  start.date = as.Date( '2004-01-01' ),
  end.date   = today() %m+% years(2), 
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

  filename = get.expiry.filename( name );
  expiries.char = readLines( filename );
  expiries = as.Date( expiries.char );
  expiries = expiries[ sd <= expiries & expiries <= ed ];
  return( expiries );
};

###########################################################################################
# Routine: save.expiries
#
# Save a .txt file for a given set of option or future expiries
###########################################################################################

save.expiries <- function( 
  name, 
  start.date = as.Date( '1950-01-01' ),
  end.date   = as.Date( '2050-01-01' )
) {

  filename = get.expiry.filename( name );

  expiries <- calc.expiries( name, start.date = start.date, end.date = end.date );
  write.table( expiries, file = filename, sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE )
};  

###########################################################################################
# Routine: get.expiries
###########################################################################################

calc.expiries <- function( 
  name, 
  start.date = as.Date( '1950-01-01' ),
  end.date   = as.Date( '2050-01-01' )
) {
  if( name == 'VIX' ) {
    expiries <- calc.VIX.expiries( start.date = start.date, end.date = end.date );
  } else if( name == 'US Options' ) {
    expiries <- calc.US.option.expiries( start.date = start.date, end.date = end.date );
  } else {
    stop( sprintf( 'No known filename for argument %s', name ) );
  };

  return( expiries );
};

###########################################################################################
# Routine: find.third.fridays
#
# Find the third Friday of every month between the start and end dates
###########################################################################################

find.third.fridays <- function( 
  start.date = as.Date( '1950-01-01' ),
  end.date   = as.Date( '2050-01-01' )
) {
  sd = as.Date( sprintf( '%d-%d-01', year( start.date ), month( start.date ) ) );
  ed = as.Date( sprintf( '%d-%d-01', year( end.date ), month( end.date ) ) ) %m+% months(1) - 1;

  first.friday <- sd + days( ( 6 - wday( sd ) ) %% 7 );
  fridays <- seq( from = first.friday, to = ed, by = '1 week' );

  # Find the first Friday of each month
  is.first.friday    <- c( 1, as.numeric( !!diff( month( fridays ) ) ) );
  third.friday.inds <- c( 0, 0, is.first.friday[-(1:2)] - is.first.friday[1:(length(fridays)-2)] ) == -1;

  third.fridays = fridays[ third.friday.inds ]; 
  return( third.fridays );
};

###########################################################################################
# Routine: calc.VIX.expiries
#
# Calculate the VIX expiry for a given month.  VIX Futures expiries are 30 days prior to the
# third Friday of the following month.  If the third Friday of the following month is a 
# CBOE holiday, then futures expire 31 days prior to the third Friday.
###########################################################################################

calc.VIX.expiries <- function(
  start.date = as.Date( '1950-01-01' ), 
  end.date   = as.Date( '2050-01-01' ), 
  holidays   = c()
) {
  ed = end.date %m+% months(1);
  if( identical( holidays, c() ) ) {
    holidays <- get.holidayNYSE( start.year = year( start.date ), end.year = year( ed ) );
  };
 
  third.fridays <- find.third.fridays( start.date = start.date, end.date = ed );

  # Expiration date is 30 days prior to the 3rd Friday of the following month
  expiries <- third.fridays - days(30);

  # If the third Friday is a holiday, then the futures expire 31 days before.  Otherwise, it is 30 days prior.
  holiday.fridays <- !is.na( match( third.fridays, holidays ) );
  expiries[ holiday.fridays ] <- expiries[ holiday.fridays ] - days(1);

  return( expiries );
};

###########################################################################################
# Routine: calc.US.option.expiries
#
# Calculate US option expiries.  US options expire on the Saturday after the third Friday 
# of each month, unless the third Friday is an exchange holiday, in which case the options
# expire on the third Friday.
###########################################################################################

calc.US.option.expiries <- function(
  start.date = as.Date( '1950-01-01' ),
  end.date   = as.Date( '2050-01-01' ),
  holidays   = c()
) {
  ed = end.date %m+% months(1);
  if( identical( holidays, c() ) ) {
    holidays <- get.holidayNYSE( start.year = year( start.date ), end.year = year( ed ) );
  };

  third.fridays <- find.third.fridays( start.date = start.date, end.date = ed );

  # Expiration date is the Saturday after the third Friday
  expiries <- third.fridays + days(1);

  # If the third Friday is a holiday, then the options expire on the third Friday
  holiday.fridays <- !is.na( match( third.fridays, holidays ) );
  expiries[ holiday.fridays ] <- expiries[ holiday.fridays ] - days(1);

  return( expiries );
};





