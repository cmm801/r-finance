
source( sprintf( "%s/VIX/download_VIX.R", DATABASE_PATH ) );

###########################################################################################
# Routine: update.VIX
#
# Download VIX futures data from the CBOE website, and write CSV file
###########################################################################################

update.VIX = function( 
  start.date = as.Date( '2003-12-31' ), 
  end.date   = today() + years(1)
) {
  NewData = download.VIX( start.date = start.date, end.date = end.date );
  filename = sprintf( "%s/downloads/VIX.csv", DATA_PATH );
  write.table( NewData, file = filename, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE );
  print( sprintf( 'Uploaded %s rows to VIX.csv', nrow( NewData ) ) );
  nrows.added = nrow(NewData);
  return(nrows.added); 
};

