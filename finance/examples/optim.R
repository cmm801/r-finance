
library( tseries );

pf_trade = initialize.trade();
pf = Portfolio( pf_trade );
pf = setUnderliers( pf );
symbols = pf@UnderlierSymbols;
weights = matrix( pf@UnderlierWeights, nrow = 1 );

# Add UNG
symbols = c( pf@UnderlierSymbols, 'UNG' )
weights = matrix( c( pf@UnderlierWeights, 0 ), nrow = 1 )

out = estimate.asset.params()
#symbols = c( "AGNC","ERX","ERY","FAS","FAZ","SDS","TNA","TZA","VXX","USDCash" );

mu = matrix( out$AdjPremia[ symbols, ], nrow = 1 );
covar = out$Covar[ symbols, symbols ];
covar[ 'USDCash', 'USDCash' ] = 1e-6;

N = length( symbols );

lowerBound = rep( -.25, N )
upperBound = rep(  .25, N )

lowerBound[ symbols == 'TVIX' ] = 0;
lowerBound[ symbols == 'USDCash' ] = -1;
upperBound[ symbols == 'USDCash' ] =  3;

sol = portfolio.optim( x = mu, pm = .10, covmat = covar, shorts = T, reslow = lowerBound, reshigh = upperBound );


df = data.frame( Symbol = symbols, Weight = round(sol$pw*1e4)/100, Mu = round(as.numeric(mu)*1e4)/100, 
						Vol = round(sqrt( diag( covar ) )*1e4)/100 );

print( mu %*% df$Weight );
print( sqrt( df$Weight %*% covar %*% df$Weight ) );
print( mu %*% t(weights) );
print( sqrt( weights %*% covar %*% t(weights) ) );



