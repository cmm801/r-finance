
source( sprintf( "%s/yahoo/upload_equity.R", DATABASE_PATH ) );

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];

nrows.added = update.dividends();
append.to.log( 'upload_dividends', today(), nrows.added );

