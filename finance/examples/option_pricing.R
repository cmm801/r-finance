
#####################################################################################
# In this script, we test that option prices achieved using several different methods
#  are identical with one another, up to floating point errors. The three methods are:
# 1. Using the lognormal distribution to find the expected values of different strikes.
# 2. Using my BS function
# 3. Using the R function EuropeanOption
# 
#####################################################################################

Spot = 1;
N = 200;
Strikes = seq( 1/2/N, 4, length = N )


sigma = .2;
mu = -sigma^2/2;
cum.probs = plnorm(Strikes, mu, sigma );

div = 0;
rfr = 0;
maturity = 1;

# Calculate option prices using the lognormal distribution
my.call.prices = option.prices.from.prob( cum.probs, Strikes = Strikes, type = 'call' );
my.put.prices = option.prices.from.prob( cum.probs, Strikes = Strikes, type = 'put' );

# Use my function for quickly calculating BS Prices
bs.put.prices = calc.bs.price( 'P', Strike = Strikes, Spot = Spot, maturity = maturity, vol = sigma, div = div, rf.rate = rfr );
bs.call.prices = calc.bs.price( 'C', Strike = Strikes, Spot = Spot, maturity = maturity, vol = sigma, div = div, rf.rate = rfr );

# Use the R function, EuropeanOption
r.put.prices = unlist( lapply( Strikes, function(x) EuropeanOption( 'put', underlying = Spot, strike = x, 
			dividendYield = div, riskFreeRate = rfr, maturity = maturity, volatility = sigma )$value ) )
r.call.prices = unlist( lapply( Strikes, function(x) EuropeanOption( 'call', underlying = Spot, strike = x, 
			dividendYield = div, riskFreeRate = rfr, maturity = maturity, volatility = sigma )$value ) )

# Find out if put and call volatilities are the same
put.prices = my.put.prices;
call.prices = my.call.prices;

put.vols = Strikes * NaN;
call.vols = Strikes * NaN;
for( i in 1:length( Strikes ) ) {
  try( { put.vols[i]=EuropeanOptionImpliedVolatility( 'put',value=put.prices[i],underlying=Spot,strike=Strikes[i],dividendYield=div, 
                        riskFreeRate=rfr,maturity=maturity, volatility = sigma )$impliedVol }, silent = TRUE );
  try( { call.vols[i]=EuropeanOptionImpliedVolatility( 'call',value=call.prices[i],underlying=Spot,strike=Strikes[i],dividendYield=div,
                        riskFreeRate=rfr,maturity=maturity,volatility = sigma )$impliedVol }, silent = TRUE );
};

par( mfrow = c(2, 1 ) )
plot( Strikes, put.vols, col = 'blue', ylim = c( 0, 2*mean( put.vols, na.rm = T ) ) );
lines( Strikes, call.vols, col = 'red' );
legend( 'top', c( 'Puts', 'Calls' ), fill=c('blue','red') )


################################################################################
# Use student T distribution to see if Put/Call vols are still the same
################################################################################

scaling = 5;
S0 = 1.1;
t.cum.probs = pt( (Strikes-S0)*scaling, df=3, ncp=0 );
t.call.prices = option.prices.from.prob( t.cum.probs, Strikes = Strikes, type = 'call' );
t.put.prices = option.prices.from.prob( t.cum.probs, Strikes = Strikes, type = 'put' );

# Find out if put and call volatilities are the same
put.prices = t.put.prices;
call.prices = t.call.prices;

Midpoints = (Strikes[-1] + Strikes[-length(Strikes)] )/2;
t.mu = 1 - sum( diff( t.cum.probs ) * Midpoints );

put.vols = Strikes * NaN;
call.vols = Strikes * NaN;
for( i in 1:length( Strikes ) ) {
  try( { put.vols[i]=EuropeanOptionImpliedVolatility( 'put',value=put.prices[i],underlying=Spot,strike=Strikes[i],dividendYield=div,
                        riskFreeRate=-t.mu,maturity=maturity, volatility = sigma )$impliedVol }, silent = TRUE );
  try( { call.vols[i]=EuropeanOptionImpliedVolatility( 'call',value=call.prices[i],underlying=Spot,strike=Strikes[i],dividendYield=div,
                        riskFreeRate=-t.mu,maturity=maturity,volatility = sigma )$impliedVol }, silent = TRUE );
};

plot( Strikes, put.vols, col = 'blue', ylim = c( 0, 2*mean( put.vols, na.rm = T ) ) );
lines( Strikes, call.vols, col = 'red' );
legend( 'top', c( 'Puts', 'Calls' ), fill=c('blue','red') )



