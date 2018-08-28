
source( paste( DATABASE_PATH, "/database.R", sep="" ) );
source( sprintf( '%s/VIX/upload_VIX.R', DATABASE_PATH ) );

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];

nrows.added = update.VIX();
append.to.log( 'upload_VIX', today(), nrows.added );

