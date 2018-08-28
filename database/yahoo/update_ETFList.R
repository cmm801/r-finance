
source( sprintf( "%s/yahoo/download_fundamental_equity.R", DATABASE_PATH ) );

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];

nrows.added = save.etf.list( path = DATA_PATH );
append.to.log( 'update_ETFList', today(), nrows.added );

