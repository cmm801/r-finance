
###########################################################################################
# Routine: report.db.status
###########################################################################################

report.db.status <- function( ndays = 7 ) {
  reporting.dates = as.Date((today()-ndays+1):today());
  reporting.dates = reporting.dates[!( wday(reporting.dates) %in% c(7,1) ) ];

  filename = sprintf( "%s/inputs/table_date_info.csv", DATA_PATH );
  table.data = read.csv( filename, sep=",", header = TRUE );
  tables = table.data$Table;

  all.info = data.frame(matrix(NA,ncol=length(reporting.dates),nrow=nrow(table.data) ) );
  colnames(all.info) = format( reporting.dates );
  rownames(all.info) = table.data$Table;

  for( i in 1:nrow( table.data ) ) {
    table     = table.data[i,'Table'];
    date.col  = table.data[i,'DateColumn'];
    date.type = table.data[i,'DateType'];

    date.counts = run.query( sprintf( 'SELECT %s, COUNT(*) as Count from %s Group by %s', date.col, table, date.col ) );
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

