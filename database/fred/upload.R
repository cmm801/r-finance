
source( paste( DATABASE_PATH, "/database.R", sep="" ) );
source( paste( DATABASE_PATH, "/fred/download.R", sep="" ) );

###########################################################################################
# Routine: prepare.fed.data
#
# Download data from FRED, but only keep new data
###########################################################################################

prepare.fed.data <- function( 
  Table = 'FREDData', 
  DB = MYSQL_DB_NAME
) {
  query = sprintf( 'SELECT Distinct Symbol FROM %s.FREDNames ORDER BY Symbol', DB );
  symbols = run.query( query )$Symbol;

  query = sprintf( "SELECT Symbol, MAX( Date ) as Date FROM %s.%s GROUP BY Symbol", DB, Table );
  last.update.dates = run.query( query, DB = DB );
  existing.symbols = last.update.dates$Symbol;

  NewData = c();
  for( symbol in symbols ) {
    print( symbol );
    symbol.data = download.fed.data( symbol );    

    if( symbol %in% existing.symbols ) {
      last.update.date = last.update.dates[ last.update.dates$Symbol == symbol, ]$Date;
      symbol.data = symbol.data[ symbol.data$Date > last.update.date, ];
    };

    print( sprintf( '%d new rows for %s', nrow( symbol.data ), symbol ) );
    NewData = rbind( NewData, symbol.data );
  };

  return( NewData );
};

###########################################################################################
# Routine: update.FREDData
#
# Update data from FRED
###########################################################################################

update.FREDData <- function(
  Table = 'FREDData',
  DB = MYSQL_DB_NAME
) {
  NewData = prepare.fed.data();
  upload.data( NewData, DB = DB, Table = Table );
  nrows.added = nrow(NewData);
  return(nrows.added);
};

