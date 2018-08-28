
source( sprintf( "%s/yahoo/upload_equity.R", DATABASE_PATH ) );

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];
MYSQL_DB_NAME = args[2];

nrows.added = update.EquityData( Table = 'EquityData', DB = MYSQL_DB_NAME );
append.to.log( 'upload_EquityData', today(), nrows.added );

