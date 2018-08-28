
###########################################################################################
# Routine: run.with.retry
#
# Attempt to run function myfun.  If function fails, attempt re-run n.retry times, and 
#  sleep between attempts.  The argument 'args' is a list of named variables, corresponding 
#  to the named arguments of myfun.
###########################################################################################

run.with.retry <- function(
  myfun,
  args,
  n.retry = 3, 
  sleep.time = 5, 
  throw.error= TRUE
) {
  ctr = 0;
  while( ctr < n.retry + 1 ) {
    result = try( do.call( myfun, args ), silent = TRUE );
    
    if( class( result ) == 'try-error' ) {
      ctr = ctr + 1;
      print( sprintf( 'Sleeping for %f seconds', sleep.time ) );
      Sys.sleep( sleep.time );
    } else {
      break;
    };
  };

  if( class( result ) == 'try-error' ) {
    if( throw.error ) {
      stop( sprintf( 'Problem running function: %s.  %s', myfun, result ) );
    } else {
      return( NULL );
    };
  };

  return( result );
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
# Routine: string2num
#
# Convert B, M, K to billions, millions, thousands respectively
###########################################################################################

string2num <- function( string.val ) {

  val = as.numeric( gsub( '[BMK]', '', string.val ) );

  if( -1 != regexpr( 'K', string.val ) ) {
    val = val * 1e3;
  } else if( -1 != regexpr( 'M', string.val ) ) {
    val = val * 1e6;
  } else if( -1 != regexpr( 'B', string.val ) ) {
    val = val * 1e9; 
  };

  return( val );
};


