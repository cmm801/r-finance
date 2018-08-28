
source( paste( HOME, "/programming/R/database/yahoo/upload_options.R", sep = "" ) );

args <- commandArgs(trailingOnly = TRUE)
ticker = args[1];
MYSQL_DB_NAME = args[2];

nrows.added = update.symbol.YahooOptions( ticker, DB = MYSQL_DB_NAME );
append.to.log( 'upload_YahooOptions', today(), nrows.added );

