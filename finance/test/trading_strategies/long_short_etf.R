
nEstimatingBeta  = 20;
nRebalancePeriod = 20;
start.date = as.Date( '2012-12-31' );
transaction.costs = .005;
rebalance.threshold = 0.1;
thirty.day.rule = TRUE;

longLeg = 'TNA';
shortLeg = 'TZA'

tvix = backtest.etf.strategy( shortLeg, longLeg, nEstimatingBeta = nEstimatingBeta, nRebalancePeriod = nRebalancePeriod, 
	start.date = start.date, transaction.costs = transaction.costs, rebalance.threshold = rebalance.threshold, 
	thirty.day.rule = thirty.day.rule );
tvix.ts = timeSeries( rowSums( tvix ) );

strat.ts = tvix.ts;
spy = get.time.series( 'SPY', start.date = first.date( strat.ts ) );

SR = SharpeRatio.annualized( ri( strat.ts ) );
CR = corr( ri( nanmerge( spy, strat.ts ) ) );


