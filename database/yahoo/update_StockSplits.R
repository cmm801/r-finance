
source( sprintf( "%s/yahoo/upload_equity.R", DATABASE_PATH ) );

nrows.added = update.stock.splits();
append.to.log( 'update_stock_splits', today(), nrows.added );

