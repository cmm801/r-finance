
source( sprintf( "%s/investing/upload_FX.R", DATABASE_PATH ) );
source( paste( DATABASE_PATH, "/database.R", sep="" ) );

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];
MYSQL_DB_NAME = args[2];

fx.nrows.added     = upload.investing.com.exchange.rates();
fx.fwd.nrows.added = upload.investing.com.forward.rates();

append.to.log( 'FX_Rates', today(), fx.nrows.added );
append.to.log( 'FX_Forwards', today(), fx.fwd.nrows.added );

