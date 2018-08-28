sigma = 0.20;
S_0   = 100;
S_f   = S_0 * exp( sigma * rnorm( 1e6, 0, 1 ) - 0.5 * sigma*sigma  );

Strikes = seq( from = S_0 / 10, to = 2*S_0, by = 5 );

call.prices  = c();
put.prices   = c();
call.vol = c();
put.vol = c();
for( i in 1:length( Strikes ) ) {
  call.prices[i] = mean( unlist( lapply( S_f, function(x) max( 0, x - Strikes[i] ) ) ) );
  put.prices[i] = mean( unlist( lapply( S_f, function(x) max( 0, Strikes[i] - x ) ) ) );
  try( {
    res = EuropeanOptionImpliedVolatility( type = "call", underlying = S_0, strike = Strikes[i], dividendYield = 0, 
		value = call.prices[i], riskFreeRate = 0, maturity = 1, volatility = sigma ) 
    call.vol[i] = res$impliedVol;
  }  , silent = TRUE );

  try( {
    res = EuropeanOptionImpliedVolatility( type = "put", underlying = S_0, strike = Strikes[i], dividendYield = 0,        
                value = put.prices[i], riskFreeRate = 0, maturity = 1, volatility = sigma )
    put.vol[i] = res$impliedVol;
  }  , silent = TRUE );
};
