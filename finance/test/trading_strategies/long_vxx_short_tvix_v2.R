

backtest.etf.strategy <- function(
  shortLeg,
  longLeg,
  nEstimatingBeta  = 20,
  nRebalancePeriod = 20,
  start.date = as.Date( '2012-12-31' ),
  transaction.costs = .005,
  rebalance.threshold = 0.05,
  thirty.day.rule = TRUE
) {

  calc.TVIX.strat.weights <- function( beta ) {
    if( beta < 0 ) {
      wts = c( beta/(1-beta), -1/(1-beta ) );
    } else {
      wts = c( beta/(1+beta), -1/(1+beta ) );
    };
    return( c( 1 - sum(wts), wts ) );
  };

  ts = get.time.series( c( 'USDCash', longLeg, shortLeg ), start.date = start.date - days( nEstimatingBeta + 2*nRebalancePeriod ) );

  reg.coef = rolling.lm( ri(ts[,shortLeg]), ri(ts[,longLeg]), N = nEstimatingBeta )
  orig.beta = reg.coef[,2];

  comb.ts = nanmerge( orig.beta, ri( ts ) );
  beta = comb.ts[,1];
  rtns = comb.ts[,-1];
  
  total.costs = 0;
  MV = matrix( calc.TVIX.strat.weights( beta[1] ), nrow = 1, ncol = 3 );
  dev= matrix( 0, nrow = 1, ncol = 1 );
  T = nrow(rtns);
  last.rebalance = 0;
  last.direction = sign(MV[1,2]);
  for( t in 2:T ) {
     MV = rbind( MV, MV[t-1,] * ( 1 + as.numeric( rtns[t,] ) ) );
  
     last.rebalance = last.rebalance + 1;
     target.wts = calc.TVIX.strat.weights( beta[t-1] );
     current.wts = MV[t,] / sum( MV[t,] );
     dev = rbind( dev, sum( abs(current.wts - target.wts)/2 ) );
     trade.direction = sign( target.wts[2] - current.wts[2] );

     trading.allowed = TRUE;
     if( thirty.day.rule && ( last.rebalance < nRebalancePeriod && trade.direction != last.direction ) ) {
       trading.allowed = FALSE;
     };

     if( abs(dev[t]) > rebalance.threshold && last.rebalance >= nRebalancePeriod && trading.allowed ) {
       cost = sum( abs(current.wts[-1] - target.wts[-1]) ) * transaction.costs * sum(MV[t,]);
       MV[t,] = ( -cost + sum(MV[t,]) ) * target.wts;
       total.costs = total.costs + cost;
       last.rebalance = 0;
       last.direction = trade.direction;
     };
  };

  strat.ts = timeSeries( MV, rownames( rtns ), names( rtns ) );
  return( strat.ts );
};



