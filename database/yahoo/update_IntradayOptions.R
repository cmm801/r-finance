
source( paste( HOME, "/programming/R/database/yahoo/upload_options.R", sep = "" ) );

args <- commandArgs(trailingOnly = TRUE)
ticker <- args[1];
MYSQL_DB_NAME <- args[2];
DATA_PATH = args[3];

update.symbol.IntradayOptions( ticker, DB = MYSQL_DB_NAME );


