
ticker = 'SPY';
numObs = 21;
numSims = 1e3;

prices = get.time.series( ticker, start.date = today() - years(12) );
rtns = ri( prices );

fit = garchFit( data = rtns );
C = coef(fit );
if( C['alpha1'] + C['beta1'] >= 1 ) {
  stop( 'Error: coefficients of GARCH add to more than one' );
};

presample = matrix( c( tail( fit@residuals, 1 ), tail( fit@h.t, 1 ), 0 ), nrow = 1 );
coefs =  list( alpha = C['alpha1'], beta = C['beta1'], omega = C['omega'] );
spec = garchSpec( model = coefs, presample = presample )
sim.rtns = garchSim( spec, n = numObs, n.start = 1 );

sim.rtns = matrix( nrow = numObs, ncol = numSims );
for( i in 1:numSims ) {
  sim.rtns[,i] = as.numeric( garchSim( spec, n = numObs, n.start = 1 ) );
};


