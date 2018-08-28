
source( sprintf( "%s/yahoo/upload_fundamental_equity.R", DATABASE_PATH ) );

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];
MYSQL_DB_NAME = args[2];

nrows.added = update.CompanyData( Table = 'CompanyData', DB = MYSQL_DB_NAME );
append.to.log( 'update_CompanyData', today(), nrows.added );

