  source( paste( BASE_PATH_FINANCE, "/LIBRARY.R", sep="" ) );

  start.date = Sys.Date() - 10 * 365;

  risk.free.rate = 'LIBOR1';
  etf.list = read.csv( '../data/levered_etfs.csv', header=FALSE, stringsAsFactors=FALSE )
 
  for( i in 1:dim( etf.list )[1] ) {
    tickers = etf.list[ i, 1:2 ];   
    ts   = get.time.series( tickers, start.date=start.date, IR.type = risk.free.rate );
    rtns = returns( ts, method = 'discrete' );
  
    nyear  = as.numeric( ( as.Date( tail( rownames( ts ), 1 ) ) - as.Date( head( rownames( ts ), 1 ) ) ) / 365.25 );
    weights = c( -0.5, -0.5, 2 ); 

    pf  = portfolio.returns( ts, weights, rebalance.period = 30 );

    print( sprintf( "%s %s", tickers[[1]], tickers[[2]] ) ); 

#    key.stats = calc.key.stats( pf[, "Total" ], periods = c( '01m', '03m', '12m', '36m' ) );
#    for( nm in sort( names( key.stats ) ) )
#      print( sprintf( "   %-12s %.2f", nm, key.stats[[ nm ]] ) );

    tot.rtn.stats = calc.key.stats( cumulated( -rtns[, tickers[[1]] ] - rtns[, tickers[[2]] ], method = 'discrete' ), 
									periods = c( '01m', '02m', '03m', '06m', '12m', '36m' ) );
    for( nm in sort( names( tot.rtn.stats ) ) ) 
      if( sum( grep( 'Rtn', nm ) ) ) 
        print( sprintf( "  %-8s %.2f", nm,  100 * tot.rtn.stats[[ nm ]] ) );
  }; 


