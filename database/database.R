
library( RMySQL, quietly = TRUE );
library( lubridate, quietly = TRUE );

###########################################################################################
# Routine: upload.data 
###########################################################################################

upload.data <- function( NewData, Table, DB = MYSQL_DB_NAME ) {
  current.time = format( Sys.time(), '%Y-%m-%d %H:%M' );
  if( is.null( NewData ) || nrow( NewData ) == 0 ) {
    print( sprintf( '%s   No rows were added to %s in %s', current.time, Table, DB ) );
  } else {
    con = get.db.connection( DB = DB );
    dbWriteTable( con, Table, NewData, overwrite = FALSE, append = TRUE, row.names = FALSE );
    print( sprintf( '%s   Uploaded %d new rows to Table %s in DB %s', current.time, nrow( NewData ), Table, DB ) );
    dbDisconnect( con );
  };
};

###########################################################################################
# Routine: get.db.connection
###########################################################################################

get.db.connection <- function( DB = MYSQL_DB_NAME ) {
  drv = dbDriver('MySQL')
  if( MYSQL_SOCKET != '' ) {
    con = dbConnect(drv,db = DB, user = 'root', pass = MYSQL_PASSWORD, unix.socket = MYSQL_SOCKET );
  } else {
    con = dbConnect(drv,db = DB, user = 'root', pass = MYSQL_PASSWORD );
  };

  return( con );
};

###########################################################################################
# Routine: run.query
###########################################################################################

run.query <- function( query, DB = MYSQL_DB_NAME ) {
  con = get.db.connection( DB = DB );
  res = dbGetQuery(con, statement = query );
  dbDisconnect( con );

  return( res );
};

###########################################################################################
# Routine: report.db.status
###########################################################################################

report.db.status <- function( 
  ndays = 7,
  DB = MYSQL_DB_NAME
) {
  reporting.dates = today() + (-ndays+1):0
  reporting.dates = reporting.dates[!( wday(reporting.dates) %in% c(7,1) ) ];

  data.info.path = sprintf( "%s/inputs/table_date_info.csv", DATA_PATH );
  table.data = read.csv( data.info.path, sep=",", header = TRUE, stringsAsFactors=FALSE );
  tables = table.data$Table;

  all.info = data.frame(matrix(NA,ncol=length(reporting.dates),nrow=nrow(table.data) ) );
  colnames(all.info) = format( reporting.dates );
  rownames(all.info) = table.data$Table;

  for( i in 1:nrow( table.data ) ) {
    table     = table.data[i,'Table'];
    date.col  = table.data[i,'DateColumn'];
    date.type = table.data[i,'DateType'];

    query = sprintf( "SELECT %s, COUNT(*) as Count from %s.%s WHERE %s > '%s' Group by %s", 
			date.col, DB, table, date.col, as.Date( today()-ndays-1 ), date.col );
    date.counts = run.query( query, DB = DB );

    if( nrow( date.counts ) == 0 ) {
      next;
    }; 

    if( date.type == 'Date' ) {
      all.info[i,] = date.counts[ match( reporting.dates, as.Date(date.counts[,date.col] ) ), 'Count' ];
    } else if( date.type == 'DateTime' ) {
      trade.dates = as.Date( date.counts$Trade_Time );
      for( k in 1:length( reporting.dates ) ) {
        reporting.date = as.Date( reporting.dates[k] );
        all.info[i,k] = sum( date.counts[ trade.dates == reporting.date, 'Count' ] );
      };
    } else {
      stop( paste( 'Unknown date type: ', date.type ) );
    };
  };

  return( all.info );
};

###########################################################################################
# Routine: get.log.path
###########################################################################################

get.log.path <- function() {
  filename = sprintf( '%s/process_log.txt', BASE_PATH );
  return( filename );
};

###########################################################################################
# Routine: read.from.log
###########################################################################################

read.from.log <- function() {
  filename <- get.log.path();
  data <- read.table( filename, header = T, stringsAsFactors = FALSE );
  data$Update.Time = as.Date( data$Update.Time );
  return( data );
};

###########################################################################################
# Routine: append.to.log
###########################################################################################

append.to.log <- function( 
  name,
  time, 
  info
) {
  old.data = read.from.log();
  old.data$Update.Time = as.Date( old.data$Update.Time );

  new.data = data.frame( Process.Name = name, Update.Time = as.Date( time ), Additional.Info = info );

  data = rbind( old.data, new.data );
  output.file = get.log.path();
  write.table( data, file = output.file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE )
};

###########################################################################################
# Routine: get.log.details
###########################################################################################

get.log.details <- function( asOfDate = Sys.Date(), nDays = 7 ) {
  log.data = read.from.log();

  uniq.processes = unique( log.data$Process.Name );
  target.dates = as.Date( (asOfDate-nDays):asOfDate, '1970-01-01' );
  
  data = matrix( NA, nrow = length(target.dates), ncol = length(uniq.processes) );
  for( p in 1:length(uniq.processes) ) {
    for( d in 1:length(target.dates) ) {
      date = target.dates[d];
      process = uniq.processes[p];
      idx = log.data$Update.Time == date & log.data$Process.Name == process;
      if( any(idx) ) {
        data[d,p] = sum( log.data$Additional.Info[idx] );            
      };
    };
  };

  data = as.data.frame(data);
  rownames(data) = target.dates;
  colnames(data) = uniq.processes;

  return( data );
};



