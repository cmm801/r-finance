
###########################################################################################
# Routine: save.time.series.prices
# 
# Save price time series data, for use with running tests
###########################################################################################

save.time.series.prices <- function(
  tickers, 
  output.file, 
  start.date, 
  end.date
) {
  prices = get.time.series( tickers, start.date = start.date, end.date = end.date, data.type = 'Close' );

  # Keep all weekdays
  prices = fill.missing( prices, start.date = start.date, end.date = end.date );
  prices = prices[ wday( rownames( prices ) ) %in% 2:6, ];

  # Save the data
  save.time.series.data( prices, output.file = output.file, start.date = start.date, end.date = end.date );
};

###########################################################################################
# Routine: save.time.series.dividends
# 
# Save price time series data, for use with running tests
###########################################################################################

save.time.series.dividends <- function(
  tickers,
  output.file,
  start.date,
  end.date
) {
  div.ts = get.dividends.ts( tickers, start.date = start.date, end.date = end.date );

  missing.tickers = setdiff( tickers, colnames( div.ts ) );
  if( length( missing.tickers ) ) {
    zero.mtx = matrix( 0, nrow = nrow( div.ts ), ncol = length( missing.tickers ) );
    dividends = merge.ts( div.ts, timeSeries( zero.mtx, rownames(div.ts), units = missing.tickers ) );
  } else {
    dividends = div.ts;
  };
 
  # Put the dividend columns into the order of tickers 
  dividends = dividends[,tickers];

  save.time.series.data( dividends, output.file = output.file, start.date = start.date, end.date = end.date );
};

###########################################################################################
# Routine: save.time.series.data
# 
# Save price time series data, for use with running tests
###########################################################################################

save.time.series.data <- function(
  ts,
  output.file,
  start.date,
  end.date
) {
  df = as.data.frame( ts );
  dates = as.Date( row.names( ts ) );
  rownames(df) = c();

  data = cbind( dates, df );
  colnames( data ) = c( 'Date', colnames(ts) );

  write.table( data, file = output.file, row.names = F, col.names = T, sep = ',' )
};

###########################################################################################
# Routine: read.time.series
#
# Read time series data that has been saved for tests
###########################################################################################

read.time.series <- function(
  tickers, 
  filename, 
  start.date, 
  end.date
) {
  ts = read.csv( filename, row.names = F, col.names = T, sep = ',' );
  ts = ts.range( ts, start.date = start.date, end.date = end.date );
  return(ts);
};

