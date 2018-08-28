
###########################################################################################
# Routine: setup.buy.and.hold
# 
# Save files and run setups necessary to update the buy and hold test
###########################################################################################

setup.buy.and.hold <- function() {
  tickers = c( 'USDCash', 'AGNC', 'TNA', 'TZA', 'VXX' );
  start.date = as.Date( '2011-11-21' );
  end.date = as.Date( '2015-12-31' );

  price.file = sprintf( '%s/test/performance/data/prices.csv', FINANCE_PATH );
  div.file = sprintf( '%s/test/performance/data/dividends.csv', FINANCE_PATH );

  save.time.series.prices( tickers, output.file = price.file, start.date = start.date, end.date = end.date );
  save.time.series.dividends( tickers, output.file = div.file, start.date = start.date, end.date = end.date );
};

###########################################################################################
# Routine: test.buy.and.hold
# 
# Run test for buy and hold
###########################################################################################

test.buy.and.hold <- function() {
  transaction.log = sprintf( '%s/test/performance/transactions/buy_and_hold.csv', FINANCE_PATH );

  pf_trade = new( 'Trade' );
  pf_trade@TransactionLog = transaction.log;

  pf_trade = setMktValTS( pf_trade );
};

