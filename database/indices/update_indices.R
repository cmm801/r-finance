
source( sprintf( '%s/indices/indices.R', DATABASE_PATH ) );

nrows.added = save.index.constituents();
append.to.log( 'upload_indices', today(), nrows.added );

