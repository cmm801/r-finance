
library( XML );
source( paste( HOME, "/programming/R/database/investing/download_FX.R", sep = "" ) );

###########################################################################################
# Routine: upload.investing.com.exchange.rates 
#
# Save recent exchange rate information from investing.com
###########################################################################################

upload.investing.com.exchange.rates <- function() {
  data = download.all.investing.com.exchange.rates();
  write.table( data, file = EXCHANGE_RATES_RECENT_CSV, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE )
  nrows.added = nrow( data );
  return( nrows.added );
};

###########################################################################################
# Routine: upload.investing.com.forward.rates
#
# Save recent FX Forward information from investing.com
###########################################################################################

upload.investing.com.forward.rates <- function() {
  new.data = download.all.investing.com.forward.rates();

  db.data = read.csv( FORWARD_RATES_RECENT_CSV, sep = '\t', header = TRUE, stringsAsFactors = FALSE );
  historic.data = db.data[ as.Date(db.data$AsOfDate) < max( as.Date(new.data$AsOfDate) ), ];
 
  data = rbind( historic.data, new.data[,colnames(historic.data) ]  ); 
  write.table( data, file = FORWARD_RATES_RECENT_CSV, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE );
  nrows.added = nrow( data );
  return( nrows.added );
};

