
args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];
MYSQL_DB_NAME = args[2];

source( sprintf( '%s/fred/upload.R', DATABASE_PATH ) );
nrows.added = update.FREDData( Table = 'FREDData', DB = MYSQL_DB_NAME );
append.to.log( 'upload_FREDData', today(), nrows.added );

