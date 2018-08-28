
nEstimatingBeta  = 20;
nRebalancePeriod = 20;
start.date = as.Date( '2012-12-31' );
transaction.costs = .005;
rebalance.threshold = 0.10;
thirty.day.rule = TRUE;

tvix = backtest.etf.strategy( 'TVIX', 'XIV', nEstimatingBeta = nEstimatingBeta, nRebalancePeriod = nRebalancePeriod, 
	start.date = start.date, transaction.costs = transaction.costs, rebalance.threshold = rebalance.threshold, 
	thirty.day.rule = thirty.day.rule );
tvix.ts = timeSeries( rowSums( tvix ) );

faz = backtest.etf.strategy( 'FAZ', 'FAS', nEstimatingBeta = nEstimatingBeta, nRebalancePeriod = nRebalancePeriod, 
	start.date = start.date, transaction.costs = transaction.costs, rebalance.threshold = rebalance.threshold, 
	thirty.day.rule = thirty.day.rule );
faz.ts = timeSeries( rowSums( faz ) );

ery = backtest.etf.strategy( 'ERY', 'ERX', nEstimatingBeta = nEstimatingBeta, nRebalancePeriod = nRebalancePeriod, 
	start.date = start.date, transaction.costs = transaction.costs, rebalance.threshold = rebalance.threshold, 
	thirty.day.rule = thirty.day.rule );
ery.ts = timeSeries( rowSums( ery ) );

tza = backtest.etf.strategy( 'TZA', 'TNA', nEstimatingBeta = nEstimatingBeta, nRebalancePeriod = nRebalancePeriod, 
	start.date = start.date, transaction.costs = transaction.costs, rebalance.threshold = rebalance.threshold, 
	thirty.day.rule = thirty.day.rule );
tza.ts = timeSeries( rowSums( tza ) );

zsl = backtest.etf.strategy( 'ZSL', 'AGQ', nEstimatingBeta = nEstimatingBeta, nRebalancePeriod = nRebalancePeriod, 
	start.date = start.date, transaction.costs = transaction.costs, rebalance.threshold = rebalance.threshold, 
	thirty.day.rule = thirty.day.rule );
zsl.ts = timeSeries( rowSums( zsl ) );

strat.ts = 1/5 * ( tvix.ts + faz.ts + ery.ts + tza.ts + zsl.ts );
spy = get.time.series( 'SPY', start.date = first.date( strat.ts ) );

SR = SharpeRatio.annualized( ri( strat.ts ) );
CR = corr( ri( nanmerge( spy, strat.ts ) ) );


