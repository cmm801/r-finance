
nEstimatingBeta  = 20;
nRebalancePeriod = 5;
start.date = as.Date( '2012-12-31' );

shortLeg = 'TVIX';
longLeg = 'VXX';

ts = get.time.series( c( 'TVIX', 'VXX', 'XIV' ), start.date = start.date - days( round( nEstimatingBeta + 2*nRebalancePeriod ) ) );
rtns = ri(ts, n = nRebalancePeriod );

reg.coef = rolling.lm( rtns[,shortLeg], rtns[,longLeg], N = nEstimatingBeta )
alpha = reg.coef[,1];
beta = reg.coef[,2];

rtn.beta = nanmerge( beta, rtns );
names( rtn.beta ) = c( 'Beta', names( rtns ) );
all.ts = rtn.beta[ seq( from = 1, to = nrow( rtn.beta ), by = nRebalancePeriod ), ];

T = nrow( all.ts );

strat.rtn = c();
for( t in 2:T ) {
   strat.rtn[t-1] = as.numeric(all.ts[t-1,'Beta']) * all.ts[t,longLeg] - all.ts[t,shortLeg];
};

strat.ts = timeSeries( c( 1, cumprod(1+strat.rtn) ), rownames( all.ts ), 'Strategy' );
spy = get.time.series( 'SPY', start.date = first.date( all.ts ) - days(1) );

tts = nanmerge(  strat.ts, spy );
corr( ri(tts) );

strategy.vol = sd( strat.rtn ) * sqrt(252/nRebalancePeriod);
strategy.rtn = ( -1 + prod( 1 + strat.rtn )^((252/nRebalancePeriod)/length(strat.rtn)) );
SR = strategy.rtn / strategy.vol;

calc.TVIX.strat.weights <- function( beta ) {
  if( beta < 1 ) {
    wts = c( beta/(1-beta), -1/(1-beta ) );
  } else {
    wts = c( beta/(1+beta), -1/(1+beta ) );
  };
  return( c( 1 - sum(wts), wts ) );
};


nEstimatingBeta = 20;
nRebalancePeriod = 20;

ts = get.time.series( c( 'USDCash', longLeg, shortLeg ), start.date = start.date - days( nEstimatingBeta + 2*nRebalancePeriod ) );

reg.coef = rolling.lm( rtns[,shortLeg], rtns[,longLeg], N = nEstimatingBeta )
orig.alpha = reg.coef[,1];
orig.beta = reg.coef[,2];

comb.ts = nanmerge( orig.beta, ri( ts ) );
beta = comb.ts[,1];
rtns = comb.ts[,-1];

MV = matrix( calc.TVIX.strat.weights( beta[1] ), nrow = 1, ncol = 3 );
dev= matrix( 0, nrow = 1, ncol = 1 );
T = nrow(rtns);
last.rebalance = 0;
threshold = 0.05;
for( t in 2:T ) {
   MV = rbind( MV, MV[t-1,] * ( 1 + as.numeric( rtns[t,] ) ) );

   last.rebalance = last.rebalance + 1;
   target.wts = calc.TVIX.strat.weights( beta[t-1] );
   current.wts = MV[t,] / sum( MV[t,] );
   dev = rbind( dev, sum( abs(current.wts) - abs(target.wts) )/2 );

   #if( last.rebalance >= nRebalancePeriod ) {
   if( abs(dev[t]) > threshold ) {
     MV[t,] = sum(MV[t,]) * target.wts;
     print( sprintf( 'Rebalanced after %d days', last.rebalance ) );
     last.rebalance = 0;
   };
};

strat.ts = timeSeries( MV, rownames( rtns ), names( rtns ) );
tot.ts = rowSums( strat.ts );
dev.ts = timeSeries( dev, rownames( rtns ) );
SharpeRatio.annualized( ri( tot.ts ) );

