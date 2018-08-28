source( sprintf( "%s/STARTUP.R", BASE_PATH_FINANCE ) )

print.pf.summary <- function( final.ts, name ) {
  Rtn = avg.return.ts( final.ts[, "Total" ] );
  Std = stddev.ts( final.ts[, "Total" ] );
  SharpeRatio = SharpeRatio.ts( final.ts[, "Total" ] );
  print( sprintf( "    %-24s: %-4.2f %-4.2f %-4.2f", name, 100 * Rtn, 100 * Std, SharpeRatio ) );
};

  source( paste( BASE_PATH_FINANCE, "/LIBRARY.R", sep="" ) );
  source( paste( BASE_PATH_FINANCE, "/selling.tails.R", sep="" ) );

  start.date = Sys.Date() - 10 * 365;

  risk.free.rate = 'LIBOR1';

  cash.ts = get.cash.ts( "LIBOR1" )
 
  unused.data = getData();
  all.data = Levels( SECURITY_DATABASE );

  all.tickers = c( "FAS", "FAZ", "ERX", "ERY", "TNA", "TZA" );
  weights = c( -0.5, -0.5, 2 );

  print( sprintf( "    %-24s %-4s %-4s %-4s", "Name", "Rtn", "Std", "Shrp" ) );
  for( i in seq( from=1, to=length( all.tickers)-1, by=2 ) ) {
    tickers = all.tickers[i:(i+1)];

    levels = merge( all.data[, tickers ], cash.ts );
    levels = levels[ !is.na( rowSums( levels ) ), ];

    nyear  = as.numeric( ( as.Date( tail( rownames( levels ), 1 ) ) - as.Date( head( rownames( levels ), 1 ) ) ) / 365.25 );
 
    print( paste( tickers[1], tickers[2] ) );

    for( dev in c( 0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.50, 1, 2 ) ) {

      pf1 = run.strategy( levels, weights, input.args = list( rebalance.period = 30, rebalance.threshold = dev ), 
										     allocation.rule = max.deviation.alloc.rules );

      pf2 = run.strategy( levels, weights, input.args = list( rebalance.period = 30, rebalance.threshold = dev, 
					post.rebalance.deviation = .03 ), allocation.rule = max.deviation.alloc.rules );

      print.pf.summary( pf1, sprintf( "Deviation %.2f (All)", 	 dev ) );
      print.pf.summary( pf2, sprintf( "Deviation %.2f (All)", 	 dev ) );
    }
  }

