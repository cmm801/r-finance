library( "reshape" );
TOL = 1e-4;
N = 20;

start.date = today() - years(10);
labels  = c( "Non-US Equity", "US Large Cap Equity", "US 20yr Bonds", "Emerging Equity", "US REITs", 
							"US Small Cap Equity", "European Equity", "Japanese Equity" );

symbols = c( "EFA", "SPY", "TLT", "EEM", "RWR", "IWM", "VGK", "EWJ" );

prices = get.time.series( symbols, start.date = start.date );
rtns   = returns( prices, method = 'discrete' );

initial.wts = rep( 1/ncol( rtns ), ncol( rtns ) );
weights = matrix( ncol = ncol( rtns ), nrow = N );
efft = list( x = rep( NaN, N ), y = rep( NaN, N ), weights = matrix( NaN, ncol = ncol( rtns ), nrow = N ) );
for( i in 1:N ) {
  print(i);
  lambda = (i-1)*1000/N + 1
  utility.fn = function(x)  -( 252 * sum( x * colMeans( rtns ) ) - lambda * x %*% cov( rtns ) %*% x );

  ui = rbind( rep( -1, ncol( rtns ) ), rep( 1, ncol( rtns ) ), diag( rep( 1, ncol( rtns ) ) ), diag( rep( -1, ncol( rtns ) ) ) );
  ci = c( -1 - TOL, 1 - TOL, rep( 0, ncol( rtns ) ), rep( -1, ncol( rtns ) ) );
  res = constrOptim( theta = initial.wts, f = utility.fn, grad = NULL, ui = ui, ci = ci );

  wts = res$par;
  efft$x[i] = sqrt( wts %*% cov( rtns ) %*% wts * 252 );
  efft$y[i] = sum( wts * colMeans( rtns ) ) * 252;
  weights[ i, ] = wts;
};

plot.weights( weights, names = symbols );

SR = colMeans( rtns ) * sqrt( 252 ) / sd( rtns );

equal.risk.weights = ( 1 / sd( rtns ) ) / sum( ( 1 / sd( rtns ) ) );
equal.SR.weights   = SR / sum( SR ); 

# symbols = c( "EFA", "SPY", "TLT", "EEM", "RWR", "IWM", "VGK", "EWJ" );
aggressive         = c( .10, .30, .30, .075,  .05,  .075, .05, .05 )
moderate           = c( .10, .15, .50, .075, .075, .10, 0, 0 )
conservative       = c( .05, .10, .70, .05,  .05,  .05, 0, 0 )

stats = rbind( 
  		      cbind( Name = 'EqualRisk',    weight.stats( equal.risk.weights, rtns ) ),
		      cbind( Name = 'EqualSR',      weight.stats( equal.SR.weights, rtns ) ), 
		      cbind( Name = 'Conservative', weight.stats( conservative, rtns ) ),
		      cbind( Name = 'Moderate',     weight.stats( moderate,     rtns ) ),
		      cbind( Name = 'Aggressive',   weight.stats( aggressive,   rtns ) ) );

