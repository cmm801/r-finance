
source( paste( HOME, "/programming/R/database/yahoo/upload_equity.R", sep = "" ) );

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH = args[1];
MYSQL_DB_NAME = args[2];

update.intraday.equity.data( Table = 'IntradayEquity', DB = MYSQL_DB_NAME, n.retry = 3, sleep.time = 3 );


