
source( sprintf( "%s/yahoo/download_fundamental_equity.R", DATABASE_PATH ) );
nrows.added = save.US.stock.lists();
append.to.log( 'update_StockList', today(), nrows.added );

