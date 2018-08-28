HOME = system( "echo $HOME", intern = TRUE );
if( HOME == "/root" )
  HOME = "/home/chris";

source( paste( HOME, "/programming/R/finance/LIBRARY.R", sep = "" ) );

equity.symbols = get.db.equity.symbols();
currency.info = read.table( EXCHANGE_RATE_INFO_CSV, sep = ',', header = TRUE, stringsAsFactor = FALSE );
currency.symbols = currency.info$Symbol;

symbols = c( equity.symbols, currency.symbols );
update.beta( symbols );

