
# Location of R scripts
source( paste( DATABASE_PATH, "/database.R", sep="" ) );
source( paste( DATABASE_PATH, "/yahoo/download_fundamental_equity.R", sep="" ) );

library( lubridate );

###########################################################################################
# Routine: update.CompanyData
#
# Download Yahoo fundamental equity data
###########################################################################################

update.CompanyData <- function(
  Table = 'CompanyData', 
  DB = MYSQL_DB_NAME,
  n.retry = 3, 
  sleep.time = 3, 
  batch.size = 200, 
  tol = 1e-5
) {
  NewData = prepare.yahoo.fundamental.equity.data( Table = Table, DB = DB, n.retry = n.retry, 
  					sleep.time = sleep.time, batch.size = batch.size );

  upload.data( NewData, Table = Table, DB = DB );
  nrows.added = nrow(NewData);
  return(nrows.added);
};

###########################################################################################
# Routine: prepare.yahoo.fundamental.equity.data 
#
# Download Yahoo Fundamental equity data
###########################################################################################

prepare.yahoo.fundamental.equity.data <- function(
  Table = "CompanyData",
  DB    = MYSQL_DB_NAME,
  n.retry = 3,
  sleep.time = 3,
  batch.size = 200,
  tol = 1e-5
) {
  new.data = download.all.equity.data( n.retry = n.retry, sleep.time = sleep.time, batch.size = batch.size );

  # Get information already in the database
  query = paste( c( "SELECT t2.* FROM ",
	    sprintf( "( SELECT Symbol, Code, MAX( Date ) as Date FROM %s.%s Group by Symbol, Code ) t1 ", DB, Table ), 
            sprintf( "LEFT JOIN %s.%s t2 ON t1.Symbol = t2.Symbol AND t1.Code = t2.Code AND t1.Date = t2.Date;", DB, Table ) ), 
						collapse = '' );
  db.data = run.query( query, DB = DB );
  if( nrow( db.data ) == 0 ) {
    return( new.data );
  } else {
    merged.data = merge( db.data, new.data, by = c( 'Symbol', 'Code' ) );

    new.merged.data = merged.data[ merged.data$Value.x != merged.data$Value.y, ];
    new.merged.data = new.merged.data[ abs( -1 + new.merged.data$Value.x / new.merged.data$Value.y ) > tol, ];
    new.merged.data = new.merged.data[,c( 'Symbol', 'Code', 'Value.y', 'Date.y' ) ];
    names( new.merged.data ) = c( 'Symbol', 'Code', 'Value', 'Date' );
    return( new.merged.data );
  };
};

