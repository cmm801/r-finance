  source( paste( BASE_PATH_FINANCE, "/LIBRARY.R", sep="" ) );

  start.date     = Sys.Date() - 10 * 365; 
  risk.free.rate = 'LIBOR1';
  tickers        = c( "VXX", "TVIX" ); 

  ts  = get.time.series( tickers, start.date=start.date, IR.type = risk.free.rate );

  pf.weights = c( 1, 0 );
  weights    = c( -pf.weights, sum( pf.weights ) + 1 );

  pf  = portfolio.returns( ts, weights, rebalance.period = 10 ); 
  SR  = SharpeRatio.annualized( calc.returns( pf[, "Total" ] ), scale = 252 );
  MDD = maxDrawdown( returns( pf[ , "Total" ], method = 'discrete' ) );

  print( sprintf( "Sharpe Ratio: %.2f", SR  ) );
  print( sprintf( "Max Drawdown: %.2f", MDD ) );

