
source( sprintf( "%s/yahoo/yahoo_insider.R", DATABASE_PATH ) );

args <- commandArgs(trailingOnly = TRUE)
MYSQL_DB_NAME = args[1];

update.insider.transactions( Table = 'InsiderTransactions', DB = MYSQL_DB_NAME );

