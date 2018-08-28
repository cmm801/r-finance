library( quantmod, quietly = TRUE );
library( NORMT3, quietly = TRUE );
library( nlstools, quietly = TRUE );
library( np, quietly = TRUE );

###########################################################################################
# Routine: calc.bs.price
#
# Calculate Black Scholes option prices.  This function is faster than the versions available,
# and supports a vector of Strikes and Spots
###########################################################################################

calc.bs.price <- function( 
  Type, 
  Strike, 
  Spot,  
  maturity, 
  vol, 
  div,
  rf.rate
) {
  F = Spot * exp( ( rf.rate - div ) * maturity );

  d1 = ( log(F/Strike) + vol^2/2 * maturity ) / ( vol * sqrt( maturity ) );
  d2 = d1 - vol * sqrt( maturity );

  if( 'c' == tolower( strsplit( Type, '' )[[1]][1] ) ) {
    price = exp( -rf.rate * maturity ) * ( F * pnorm(d1) - Strike * pnorm(d2) );
  } else if( 'p' == tolower( strsplit( Type, '' )[[1]][1] ) ) {
    price = exp( -rf.rate * maturity ) * ( -F * pnorm(-d1) + Strike * pnorm(-d2) );
  } else {
    stop( paste( 'Unknown option type: ', Type ) );
  };

  return( price );    
};

###########################################################################################
# Routine: calc.bs.vol
#
# Calculate Black Scholes option implied volatility  This function is faster than the versions available,
# and supports a vector of Strikes and Spots
###########################################################################################

calc.bs.vol <- function(
  Type,
  Strike,
  Spot,
  maturity,
  price, 
  div,
  rf.rate, 
  vol.search.range = c(0,3)
) {

  N = max( length( Strike ), length( Spot ), length( maturity ), length( div ), length( rf.rate ), length( price ) );

  vol = NaN * numeric(N);
  for( i in 1:N ) {
    K = ifelse( length( Strike ) == 1, Strike, Strike[i] );
    S = ifelse( length( Spot ) == 1, Spot, Spot[i] );
    d = ifelse( length( div ) == 1, div, div[i] );
    R = ifelse( length( rf.rate ) == 1, rf.rate, rf.rate[i] );
    P = ifelse( length( price ) == 1, price, price[i] );
    T = ifelse( length( maturity ) == 1, maturity, maturity[i] );

    vol.fun = function(vol) -P + calc.bs.price( Type = Type, Strike = K, Spot = S, maturity = T, 
						vol = vol, div = d, rf.rate = R );

    try( {
      result = uniroot( vol.fun, vol.search.range );
      vol[i] = result$root;
    }, silent = TRUE );

  };

  return( vol );
};

###########################################################################################
# Routine: calc.bs.greeks
#
# Calculate Black Scholes option prices.  This function is faster than the versions available,
# and supports a vector of Strikes and Spots
###########################################################################################

calc.bs.greeks <- function(
  greek.type,
  Type,
  Strike,
  Spot,
  maturity,
  vol = NA,
  price = NA,
  div = 0,
  rf.rate
) {
  eps = 1e-4;

  if( is.na(vol) && is.na(price) ) {
    print( vol )
    print( price )
    stop( 'Must specify a value for either vol or price' );
  } else if( is.na(vol) ) {
    vol = calc.bs.vol( Type = Type, Strike = Strike, Spot = Spot, maturity = maturity, price = price, div = div, 
								rf.rate = rf.rate );
  };

  if( greek.type == 'gamma' ) {
    p1 = calc.bs.greeks( 'delta', Type = Type, Strike = Strike, Spot = Spot*(1-eps), maturity = maturity, vol = vol,
								div = div, rf.rate = rf.rate );
    p2 = calc.bs.greeks( 'delta', Type = Type, Strike = Strike, Spot = Spot*(1+eps), maturity = maturity, vol = vol,
								div = div, rf.rate = rf.rate );
    gamma = (p2-p1)/(2*Spot*eps);
    return( gamma );
  };

  s1 = s2 = Spot;
  m1 = m2 = maturity;
  v1 = v2 = vol;
  r1 = r2 = rf.rate;

  if( greek.type == 'delta' ) {
    s1 = Spot*(1-eps);
    s2 = Spot*(1+eps);
    N = 2*Spot*eps;
  } else if( greek.type == 'theta' ) {
    m1 = maturity*(1+eps);
    m2 = maturity*(1-eps);
    N = 2*maturity*eps;
  } else if( greek.type == 'vega' ) {
    v1 = vol*(1-eps);
    v2 = vol*(1+eps);
    N = 2*vol*eps;
  } else if( greek.type == 'rho' ) {
    r1 = rf.rate*(1-eps);
    r2 = rf.rate*(1+eps);
    N = 2*rf.rate*eps;
  } else {
    stop( sprintf( 'Unknown Greek type: %s', greek.type ) );
  };

  p1 = calc.bs.price( Type = Type, Strike = Strike, Spot = s1, maturity = m1, vol = v1, div = div, rf.rate = r1 );
  p2 = calc.bs.price( Type = Type, Strike = Strike, Spot = s2, maturity = m2, vol = v2, div = div, rf.rate = r2 );

  greek = (p2-p1)/N;
  return( greek );
};


###########################################################################################
# Routine: create.proxy.option.ts
#
# Get Prices or Greeks for an option from Black Scholes
###########################################################################################

create.proxy.option.ts <- function( 
  Symbol, 
  Type = NA, 
  Strike = NA,
  Expiration.Date = NA, 
  rf.rate = 0,
  start.date = today() %m-% years(1), 
  end.date   = last.biz.date( today() ),  
  nDays.rolling = 126, 
  data.type = 'value',
  min.tick.size = 0
) {
  if( is.option.symbol( Symbol ) ) { 
    Option.Symbol = Symbol;
    info = parse.option.symbol( Option.Symbol );
    underlier.symbol = info$Symbol;
    Strike = info$Strike;
    Type   = info$Type;
    Symbol = info$Symbol;
    Expiration.Date = info$Expiration.Date;
    curr.div.yield = get.dividend.yield( underlier.symbol );
  } else {
    underlier.symbol = Symbol;
    Option.Symbol = create.option.symbol( underlier.symbol, Strike = Strike, Type = Type, Expiration.Date = Expiration.Date );
  };

  if( start.date > Expiration.Date ) {  
    return( timeSeries( NA, start.date, Option.Symbol ) )
  };
 
  close.prices = get.time.series( underlier.symbol, start.date = start.date - days( round( nDays.rolling*2) ), end.date = end.date, 
												data.type = 'Close' );
  adj.prices = get.time.series( underlier.symbol, start.date = start.date - days( round( nDays.rolling*2) ), end.date = end.date, 
												data.type = 'AdjClose' );
  # Restrict the rolling Volatility to start at the start date, and remove all NA's
  vol = rolling.sd( adj.prices, N = nDays.rolling );
  vol = vol[ !is.na(vol), ];
  vol = fill.missing( vol, start.date = start.date, end.date = end.date );
  vol = ts.range( vol, start.date = start.date, end.date = end.date );

  # Only price the option for dates up to the Expiration Date. 
  dates = as.Date( rownames( vol ) );
  pricing.dates = dates[ dates <= Expiration.Date ];
  maturity = as.numeric( Expiration.Date ) - as.numeric( pricing.dates );

  # Figure out the dollar value of the dividend
  curr.spot = as.numeric( get.last.price( underlier.symbol ) );
  curr.dollar.div = curr.spot * curr.div.yield;

  values = numeric( length( pricing.dates ) ) * 0;
  for( d in 1:length( pricing.dates ) ) {
    spot = find.last.date.value( close.prices, pricing.dates[d], rule = 2 )
    # Assume that the divident was historically the same, in dollar terms
    div.yield = curr.dollar.div / spot;

    if( data.type == 'value' ) {
      values[d] = calc.bs.price( Type, Spot = spot, Strike = Strike, maturity = maturity[d]/365, div = div.yield, 
									vol = vol[d], rf.rate = rf.rate );
    } else {
      values[d] = calc.bs.greeks( greek.type = data.type, Type = Type, Spot = spot, Strike = Strike, div = div.yield,
					maturity = maturity[d]/365, vol = vol[d], rf.rate = rf.rate );
    };
  };

  # Don't allow option prices to be less than 'min.tick.size'.  This prevents other functions
  # from thinking that the price differences are significant when option prices are close to 0
  values.ts = timeSeries( pmax( values, min.tick.size ), pricing.dates, units = Option.Symbol );
  return( values.ts );
};

###########################################################################################
# Routine: get.option.proxy.ts
###########################################################################################

get.option.proxy.ts <- function(
  Option.Symbols = NULL,
  start.date = today() %m-% years(3),
  end.date = last.biz.date(),
  rm.na = TRUE
) {
  proxy.ts = get.proxy.ts( Option.Symbols, start.date = start.date, end.date = end.date, 'Option', rm.na = rm.na );
  parsed.data = parse.option.symbols( Option.Symbols );

  for( i in 1:length( Option.Symbols ) ) {
    # Extract the underlier and expiration date information from the Option Symbol
    Option.Symbol = parsed.data$Option.Symbol[i];
    underlier = parsed.data$Symbol[i];
    expiry.date = parsed.data$Expiration.Date[i];

    # Get the current proxy time series, and its end date
    if( !is.empty( proxy.ts ) && nrow( proxy.ts ) > 0 ) {
      option.ts = proxy.ts[,i];
      firstProxyDate = min( as.Date( rownames( option.ts[ !is.na( option.ts ),] ) ) );
      lastProxyDate = max( as.Date( rownames( option.ts[ !is.na( option.ts ),] ) ) );
    } else { 
      firstProxyDate = as.Date(0);
      lastProxyDate = as.Date(0);
    };

    # Don't recalculate the proxy if the underlier data is no more recent than the database data
    lastDBDate = as.Date( last.db.date( underlier )$Date );

    # Don't recalculate the proxy if the proxy ts ends because of a stock split
    split.ts = get.stock.split.ts( underlier, start.date = 1 + firstProxyDate, end.date = end.date );

    # effectiveEndDate is how far the time series should extend, all things considered
    effectiveEndDate = min( end.date, lastDBDate, last.biz.date( expiry.date ) );

    # If the proxy does not extend as far as it should, then recalculate it
    if( lastProxyDate < effectiveEndDate ) {
      new.proxy = create.option.proxies( Option.Symbol, start.date = start.date, end.date = effectiveEndDate );
      if( nrow( new.proxy ) > 1 ) {
        if( !is.empty( proxy.ts ) && nrow( proxy.ts ) > 0 ) {
          proxy.ts = merge( proxy.ts[,-i], new.proxy );
        } else {
          proxy.ts = new.proxy;
        };
      } else {};
    } else {};
  };

  # Reorder the time series before returning the result
  proxy.ts = proxy.ts[,Option.Symbols];
  return( proxy.ts );
};

###########################################################################################
# Routine: create.option.proxies
# 
# Create proxy option symbols for all 
###########################################################################################

create.option.proxies <- function( 
  Option.Symbols = NULL, 
  start.date = today() %m-% years(10), 
  end.date = today(), 
  BidAsk = 'Mid',
  min.tick.size = 0.05, 
  transaction.log = TRANSACTIONS_CSV
) {
  # Get portfolio quantities
  qtys = get.pf.quantities( transaction.log = transaction.log );

  if( is.null( Option.Symbols ) ) {
    Symbols = colnames( qtys );
    Option.Symbols = Symbols[ is.option.symbol( Symbols ) ];
  };

  opt.qtys = qtys[, colnames( qtys ) %in% Option.Symbols ];
  start.dates = end.dates = c();
  for( i in 1:length( Option.Symbols ) ) {
    qty = fill.missing( opt.qtys[,i], end.date = end.date );
    initial.date = first.date( qty[ qty != 0, ] );

    # Find if there have been any stock splits before the option's expiration date
    parsed.info = parse.option.symbol( Option.Symbols[i] ); 
    split.ts = get.stock.split.ts( parsed.info$Symbol, start.date = initial.date, end.date = end.date );
    final.date = as.Date( parsed.info$Expiration.Date );

    # Save the start and end dates into an array for each option
    start.dates[i] = min( as.Date( start.date ), as.Date( initial.date ) );
    end.dates[i] = min( as.Date( end.date ), as.Date( final.date ) );
  };

  proxies = list();
  for( i in 1:length( Option.Symbols ) ) {  

    SD = as.Date( start.dates[i] );
    ED = as.Date( end.dates[i] );
    print( Option.Symbols[i] );
    proxies[[i]] <- proxy.option.price.ts( Option.Symbols[i], BidAsk = BidAsk, start.date = SD, end.date = ED );
  };
  
  proxy.ts = merge.ts( proxies );
  colnames( proxy.ts ) = Option.Symbols;

  return( proxy.ts );
};

###########################################################################################
# Routine: estimate.option.leverage
# 
# Estimate how much leverage the option provides on the underlier.  The formula is
#       Leverage = delta * ( P_{Underlier} / P_{option} )
###########################################################################################

estimate.option.leverage <- function(
  Option.Symbols,
  start.date = today() %m-% years(1),
  asOfDate = last.biz.date()
) {
  TOL = .01;
  output = c();
  for( Option.Symbol in Option.Symbols ) {

    # Get Option information (expiry date, strike, etc.)
    info = parse.option.symbol( Option.Symbol );

    if( info$Expiration.Date < start.date ) {
      # If the option expired before the asOfDate, then the leverage is undefined
      option.leverage = timeSeries( c( 0, 0 ), c( start.date, asOfDate ), units = Option.Symbol );
    } else {
      option.price = create.proxy.option.ts( Symbol=Option.Symbol, start.date=start.date, end.date = asOfDate, data.type = 'value' );
      colnames( option.price ) = 'Price';

      option.delta = create.proxy.option.ts( Symbol=Option.Symbol, start.date=start.date, end.date = asOfDate, data.type = 'delta' );
      colnames( option.delta ) = 'Delta';

      Market = get.time.series( info$Symbol, start.date = start.date, end.date = asOfDate );
      colnames( Market ) = 'Underlier';

      ts = merge.ts( list( Market, option.price, option.delta ) );
      ts = ts[ !is.na( rowSums( ts ) ), ];

      option.leverage = timeSeries( ts$Delta * ts$Underlier / ( ts$Price + TOL ), rownames( ts ), units = Option.Symbol );
    };

    output = merge.ts( list( output, option.leverage ) );
  };

  return( output );
};

###########################################################################################
# Routine: estimate.option.beta
# 
# Estimate how much the option varies with movements in the Market Index.  This is approximated
# by using the beta of the underlier, as well as the delta of the option.  The formula is
#       Beta_option = beta_{Underlier} * delta * ( P_{Underlier} / P_{option} )
###########################################################################################

estimate.option.beta <- function( 
  Option.Symbols, 
  start.date = today() %m-% years(1),
  asOfDate = last.biz.date(), 
  factor.list = 'MKT'
) {
  if( factor.list != 'MKT' ) {
    stop( 'Function estimate.option.beta is not currently supported for factor lists other than MKT' );
  };

  option.leverage = estimate.option.leverage( Option.Symbols, start.date = start.date, asOfDate = asOfDate );

  option.beta = option.leverage * NA;
  for( Option.Symbol in Option.Symbols ) {
    info = parse.option.symbol( Option.Symbol );
    beta = get.beta( info$Symbol, factor.list = factor.list );
    option.beta[, Option.Symbol ] = as.numeric( beta ) * option.leverage[, Option.Symbol ];
  };

  return( option.beta );
};


###########################################################################################
# Routine: backout.rates
#
# Backout interest rates from option price data
###########################################################################################

backout.rates <- function( 
  symbol, 
  asOfDate = last.biz.date(), 
  num.strikes.in.regression = 10
) {
  opt.data = get.option.data( symbol, asOfDate = asOfDate );
  opt.data = opt.data[ order( opt.data$Strike ), ];

  yearMonths = sort( unique( opt.data$yearMonth ) );

  spot = get.time.series( symbol, start.date = asOfDate, end.date = asOfDate )[1];

  rate = c()
  expiries = c();
  for( yearMonth in yearMonths ) {
    expiry = get.option.expiries( yearMonth = yearMonth );
    T = ( as.numeric( expiry ) - as.numeric( asOfDate ) ) / 365.25;

    put.data  = unique( opt.data[ opt.data$Type == 'P' & opt.data$yearMonth == yearMonth, ] );
    call.data = unique( opt.data[ opt.data$Type == 'C' & opt.data$yearMonth == yearMonth, ] );

    strikes = sort( intersect( put.data$Strike, call.data$Strike ) );
    strikes.near.money = strikes[ order( abs( strikes - spot ) ) ];
    S.for.regression   = sort( strikes.near.money[ 1:min( num.strikes.in.regression, length( strikes.near.money ) ) ] );

    ask.diff = put.data[ put.data$Strike %in% S.for.regression, 'Ask' ] - call.data[ call.data$Strike %in% S.for.regression, 'Ask']; 
    bid.diff = put.data[ put.data$Strike %in% S.for.regression, 'Bid' ] - call.data[ call.data$Strike %in% S.for.regression, 'Bid']; 
   
    X = c( ask.diff, bid.diff );
    Y = c( S.for.regression, S.for.regression );
   
    reg = lm( Y ~ X );
    intercept = reg$coef[[1]];

    print( yearMonth );
    print( intercept ); 
    print( T );

    rate     = c( rate, 1 / T * log( spot / intercept  ) );
    expiries = c( expiries, expiry );
  };

  df = data.frame( yearMonth = yearMonths, Rate = rate, Expiry = as.Date( expiries ) );
  return( df );
};



###########################################################################################
# Routine: simple.option.payoff
#
# Calculate the payoff for a call or put option.
# K is the spot / strike
###########################################################################################

simple.option.payoff <- function( 
  rtns, 
  K, 
  put.call, 
  output.distribution = FALSE
) {
  payoff = K * NaN; 

  for( i in 1:length( K ) ) {
    if( put.call == 'P' ) { 
      payoff[i] = simple.put.payoff( rtns, K[i], output.distribution = output.distribution ); 
    } else if( put.call == 'C' ) { 
      payoff[i] = simple.call.payoff( rtns, K[i], output.distribution = output.distribution ); 
    } else {
      stop( paste( 'Unsupported put.call type: ', put.call ) );
    }
  };

  return( payoff );
};

###########################################################################################
# Routine: simple.call.payoff
#
# Calculate the payoff for a call option.
# K is the spot / strike
###########################################################################################

simple.call.payoff <- function( 
  rtns, 
  K, 
  output.distribution = FALSE
) {
  payoff = pmax( as.numeric( ( 1 + rtns ) - K ), 0 );

  if( output.distribution == FALSE )
    payoff = mean( payoff );

  return( payoff );
}

###########################################################################################
# Routine: simple.put.payoff
#
# Calculate the payoff for a put option.
# K is the spot / strike
###########################################################################################

simple.put.payoff <- function( 
  rtns, 
  K , 
  output.distribution = FALSE
) {
  payoff = pmax( as.numeric( K - ( 1 + rtns ) ), 0 );

  if( output.distribution == FALSE )
    payoff = mean( payoff );

  return( payoff );
};

###########################################################################################
# Routine: calc.option.prices
#
# Given levels, calculate option prices with historical or bootstrapped data
# nbiz.days is the number of business days until maturity
###########################################################################################

calc.option.prices <- function( 
  levels,
  strikes, 
  put.call, 
  nbiz.days, 
  symbol        = '', 
  initial.state = -1, 
  sim.rtns      = c(), 
  spot          = 1, 
  method        = 'Markov', 
  block.size    = MARKOV_BLOCK_SIZE, 
  num.sims      = 1e4, 
  dividendYield = NA
) {
  output = data.frame();

  if( identical( sim.rtns, c() ) ) {
    rtns = simulate.returns( levels = levels, symbol = symbol, ndays = nbiz.days, method = method, block.size = block.size, 
						num.sims = num.sims, initial.state = initial.state );
  } else {
    rtns = sim.rtns 
  };
  
  for( pc in put.call ) {
    for( m in 1:length( nbiz.days ) ) {
      prices = simple.option.payoff( rtns[ m, !is.na( rtns[ m, ] ) ], K = strikes / spot, put.call = pc ) * spot;

      output = rbind( output, data.frame( Strike = strikes, Price = prices, Method = method, Type = pc, 
											nDays = nbiz.days[m], Spot = spot ) );
    };
  }

  return( output );
};

###########################################################################################
# Routine: compare.option.prices
#
# Compare quoted option prices with historical and bootstrapped prices
###########################################################################################

compare.option.prices <- function( 
  symbol, 
  asOfDate       = last.option.date( symbol ), 
  price.method   = 'Markov', 
  initial.state  = -1
) {
  # Get the option data (prices, strikes, expiries ) from the database
  DB = MYSQL_DB_NAME;
  Table = "YahooOptions";
  query    = sprintf( "SELECT * FROM %s.%s WHERE Symbol = '%s' AND Date = '%s'", DB, Table, symbol, asOfDate );
  opt.data = run.query( query, DB = DB );

  if( length( opt.data ) == 0 ) {
     return( data.frame() );
  };

  # Remove any possible duplicate rows
  opt.data = unique( opt.data );

  # Only look at options up to two years out
  opt.data = opt.data[ opt.data$yearMonth <= ( year( asOfDate ) + 2 )* 100 + month( asOfDate ), ];

  # If the Symbol pertains to a levered ETF, then construct the proxy series to use for simulating returns
  levels = get.time.series( symbol, use.proxy = TRUE );

  spot   = get.time.series( symbol, start.date = asOfDate, end.date = asOfDate )[1];
  vol.1m = sd( tail( ri( levels ) ), 21 ) * sqrt( 252 );

  # Determine the number of business days until expiry for each option
  yearMonths = unique( sort( opt.data$yearMonth ) );
  expiries   = get.expiries( 'US Options', start.ym = min( yearMonths ), end.ym = max( yearMonths ) );

  nday.sims  = unlist( lapply( expiries, function( expiry ) length( get.all.biz.days( asOfDate + days(1), expiry ) ) ) );
  
  mid            = ( opt.data$Ask + opt.data$Bid ) / 2;
  bid.ask.spread = ( opt.data$Ask - opt.data$Bid ) / ( opt.data$Bid + opt.data$Ask ) * 2;
  output = cbind( opt.data, nDays = nday.sims[ match( opt.data$yearMonth, yearMonths ) ], Spot = spot, RealizedVol.1m = vol.1m, 
										Bid.Ask.Spread = bid.ask.spread, Mid = mid );

  uniqID = sprintf( "%s %.2f %d", output$Type, output$Strike, output$nDays );

  strikes = sort( unique( output$Strike ) );

  for( method in c( 'Markov', 'Block', 'Historical' ) ) {
    sim.rtns = c();

    if( method == 'Markov' && file.exists( get.simulation.filename( symbol, 1 ) ) )
      sim.rtns <- simulate.returns( symbol = symbol, ndays = nday.sims );

    prices = calc.option.prices( levels, sim.rtns = sim.rtns, strikes = strikes, spot = spot, put.call = c( 'P', 'C' ), 
											nbiz.days = nday.sims, method = method );

    rowID  = sprintf( "%s %.2f %d", prices$Type, prices$Strike, prices$nDays );
    output = cbind( output, NewCol = prices[ match( uniqID, rowID ), 'Price' ] );

    colnames( output )[ colnames( output ) == 'NewCol' ] = method;
  };

  relative.value = output[[ price.method ]] / output$Ask;
  ann.return = -1 + relative.value^( 252 / output$nDays );

  output = cbind( output, Relative.Value = relative.value, Annualized.Return = ann.return );
  output = output[ output$nDays > 0, ];

  return( output );
};

###########################################################################################
# Routine: add.greeks
#
# Add columns with the option greeks to the data frame
###########################################################################################

add.greeks <- function( 
  opt.data, 
  divYield, 
  rf.rate, 
  include.SPX.greeks = TRUE
) {
  output = data.frame();

  symbols = unique( opt.data$Symbol );
 
  for( symbol in symbols ) { 

    temp = opt.data[ opt.data$Symbol == symbol, ];
    greeks = data.frame();

    for( i in 1:nrow( temp ) ) {
      row = temp[i, ];
      opt.type = ifelse( row$Type == 'C', 'call', 'put' );

      greeks = rbind( greeks, calc.option.greeks( opt.type = opt.type, price = row$Mid, spot = row$Spot,
                      strike = row$Strike, div = divYield, rf.rate = rf.rate, maturity = row$nDays / 252 ) );
    };

    temp = cbind( temp, greeks );

    if( include.SPX.greeks )
      temp = cbind( temp, calc.SPX.greeks( greeks[, -1], symbol ) );

    output = rbind( output, temp );
  };

  return( output );
};

###########################################################################################
# Routine: calc.SPX.greeks
#
# Compare bootstrap/historical option prices to current prices
###########################################################################################

calc.SPX.greeks <- function( 
  greeks, 
  symbol, 
  market.symbol = '^GSPC'
) {
  ts   = get.time.series( c( symbol, market.symbol ), start.date = today() %m-% years(10) );
  rtns = ri( ts );

  reg   = lm( rtns[, 1 ] ~ rtns[, 2 ] + I( rtns[, 2]^2 ) );
  coefs = as.numeric( reg$coefficients );

  beta      = coefs[2];
  convexity = coefs[3];
 
  reg2 = lm( rolling.sd( ts[, 1 ], 21 ) ~ rolling.sd( ts[, 2 ], 21 ) );
  veg  = as.numeric( reg2$coefficients )[2];

  SPX.greeks = data.frame( SPX.Delta = beta * greeks$Delta, SPX.Gamma = beta^2 * greeks$Gamma + greeks$Delta * convexity, 
					SPX.Vega = veg * greeks$Vega, SPX.Theta = greeks$Theta );

  return( SPX.greeks );
};

###########################################################################################
# Routine: calc.option.premia
#
# Compare bootstrap/historical option prices to current prices
###########################################################################################

calc.option.premia <- function( 
  symbol = '',
  asOfDate  = last.option.date( symbol ), 
  opt.type  = 'Put', 
  opt.comp  = data.frame(), 
  method    = 'Markov', 
  xlim      = c( 0.6, 1.4 ), 
  ylim      = c( 0, 4 )
) {
  price_col = method;

  if( identical( opt.comp, data.frame() ) )
    opt.comp = compare.option.prices( symbol, asOfDate = asOfDate );

  yearMonths = sort( unique( opt.comp$yearMonth ) );
  spot       = opt.comp$Spot[1]; 

  labels = c();
  nums   = c();
  first.plot = TRUE;
  for( i in 1:length( yearMonths ) ) {
    yearMonth = yearMonths[i];
    data = opt.comp[ opt.comp$Type == substr( opt.type, 1, 1 ) & opt.comp$yearMonth == yearMonth, ];

    if( first.plot == TRUE ) {
      plot( data$Strike, data[[ price_col ]] / data$Ask, xlim = xlim * spot, ylim = ylim, col = i, pch = i, 
						xlab = 'Strike', ylab = paste( price_col, '/ Ask' ), main = paste( symbol, opt.type ) );
      first.plot = FALSE;
    } else {
      points( data$Strike, data[[ price_col ]] / data$Ask, col = i, pch = i );
    };

    labels = c( labels, sprintf( '%s %d', opt.type, yearMonth ) );
    nums = c( nums, i );
  }; 
  
  abline( v = spot, lty = 2, col = 'red' ); 
  abline( h = 1,    lty = 2, col = 'red' );
  legend( "right", "top", legend = labels, col = nums, pch = nums );
};

###########################################################################################
# Routine: screen.option.prices
#
# For a set of symbols, compare current ask prices with simulations from historical data
###########################################################################################

screen.option.prices <- function( 
  symbols, 
  opt.comp         = c(), 
  price.method     = 'Markov', 
  asOfDate         = last.option.date( symbols ), 
  min.open.int     = 500, 
  min.ask          = 0.25, 
  min.pct.spot     = .01, 
  min.days.to.mat  = 21, 
  max.bid.ask.spr  = 1, 
  num.outputs      = 10 * length( symbols ), 
  rank.col         = 'Relative.Value', 
  output.file      = '', 
  include.greeks   = TRUE
) {
  data = data.frame();

  if( identical( opt.comp, c() ) ) {
    for( symbol in symbols )
      opt.comp = rbind( opt.comp, compare.option.prices( symbol, asOfDate = asOfDate, price.method = price.method ) );
  };
 
  if( length( opt.comp ) == 0 )
    return( data.frame() )  

  data = opt.comp[   min.ask          <= opt.comp$Ask      
   		   & min.pct.spot     <= opt.comp$Ask / opt.comp$Spot
   		   & min.days.to.mat  <= opt.comp$nDays 
		   & !is.na( opt.comp$Bid.Ask.Spread )
		   & max.bid.ask.spr  >= opt.comp$Bid.Ask.Spread
 		   & min.open.int     <= opt.comp$OpenInt, ];
  
  ordered.data = data[ order( data[[ rank.col ]] ), ]
  
  output = rbind( head( ordered.data, round( num.outputs / 2 ) ), tail( ordered.data, round( num.outputs / 2) ) );

  if( nrow( output ) == 0 )
    return( output );

  output = unique( output );

  if( include.greeks )
    output = add.greeks( output );

  if( output.file != '' ) {
    filename = sprintf( '%s/%s', DATA_PATH, output.file );  
    write.table( output, file = filename, sep = '\t', row.names = FALSE, quote = FALSE )
  };
  
  return( output );
};

###########################################################################################
# Routine: format.option.screen
#
# Change the order, names, and scalings of columns in the option screen
###########################################################################################

format.option.screen <- function( 
  output
) {
  basic.cols = c( 'Symbol', 'Type', 'yearMonth', 'Strike', 'K', 'Ask', 'Bid.Ask.Spread', 'Annualized.Return', 'Markov', 'Block', 'Historical' );

  greek.cols = c( 'Delta', 'Gamma', 'Vega', 'Theta' );  
  SPX.greek.cols = paste( 'SPX', greek.cols, sep = '.' );
 
  calc.cols  = c( 'RealizedVol.1m', 'ImpliedVol', greek.cols, SPX.greek.cols );

  for( price in c( 'Markov', 'Historical', 'Block' ) )
     output[ price ] = output[ price ] / output$Ask;

  output$Ask = output$Ask / output$Spot;
  output = cbind( output, data.frame( K = output$Strike / output$Spot ) ); 
  output[, c( basic.cols, calc.cols ) ];
};

###########################################################################################
# Routine: calc.implied.prob
#
# Download option data from Yahoo! and calculate the implied probability
###########################################################################################

calc.implied.prob <- function( 
  ticker, 
  yearMonth, 
  cum         = TRUE, 
  clean.data  = TRUE, 
  smooth.data = TRUE,
  std.threshold = 2
) {
  TOLERANCE = 1e-10;
  spot = as.numeric( get.last.price( ticker ) );

  opt.data  = get.option.data( ticker, yearMonth );

  raw.data = data.frame( Strikes = c(), ImpliedProb = c() );
  for( opt.type in c( "C", "P" ) ) {
    data = opt.data[ opt.data$Type == opt.type, ]; 

    if( clean.data == TRUE ) {
      res = smooth.option.prices( data$Strike, data$Ask, opt.type = opt.type, Forward = spot ); 
      strikes = res$Strike;
      asks    = res$Price;
    } else {
      strikes = data$Strike;
      asks    = data$Ask
    };

    # Use the formula for the cumulative probability density of a put/call option
    if( opt.type == "C" ) {
       probs         = 1 + c( diff( asks ) / diff( strikes ) );
       strike.midpts = c( ( strikes[ -length( strikes ) ] + strikes[ -1 ] ) / 2 );
  
       # Add additional 'Fake' points so that we can recover all of the previous option prices
       probs   	     = c( 0, probs, 1 );
       strike.midpts = c( 0, strike.midpts, ( 3*tail( strikes, 1 ) - tail( strikes, 2 )[1] ) / 2 );
    } else {
       probs         = c( 0, diff( asks ) / diff( strikes ) )
       strike.midpts = c( 0, ( strikes[ -length( strikes ) ] + strikes[ -1 ] ) / 2 );
    };
  
    raw.data = rbind( raw.data, data.frame( Strikes = strike.midpts, ImpliedProb = probs ) );
  };

  if( clean.data == TRUE ) 
    raw.data = raw.data[ TOLERANCE <= raw.data$ImpliedProb & raw.data$ImpliedProb <= 1 + TOLERANCE, ];

  if( clean.data == TRUE || smooth.data == TRUE ) {
    Strikes      = sort( unique( raw.data$Strikes ) );
    ImpliedProb = c();
    for( s in Strikes )
      ImpliedProb = c( ImpliedProb, mean( raw.data$ImpliedProb[ raw.data$Strikes == s ] ) );
  } else {
    Strikes     = raw.data$Strikes;
    ImpliedProb = raw.data$ImpliedProb;
  };

  if( clean.data == TRUE ) {
     output.data = clean.cum.prob.data( Strikes, ImpliedProb, std.threshold = std.threshold  )
  } else {
     output.data = data.frame( Strike = Strikes, ImpliedProb = ImpliedProb );
  };

  if( smooth.data == TRUE ) {
     for( i in 1:length( output.data$ImpliedProb ) ) {
        output.data$ImpliedProb[i] = max( 0, output.data$ImpliedProb[i], mean( output.data$ImpliedProb[ max(1, i-5 ):(i-1) ] ) );
        output.data = rbind( output.data, data.frame( Strike = 0, ImpliedProb = 0 ) );
    }; 

    smoothed = ksmooth( output.data$Strike, output.data$ImpliedProb, bandwidth = 2, range = c( 0, max( output.data$Strike ) ) );
    output.data = data.frame( Strike = smoothed$x, ImpliedProb = na.locf( smoothed$y ) );
  };
  
  if( cum == FALSE ) {
    Strike      = ( output.data$Strike[ -length(output.data$Strike ) ] + output.data$Strike[-1] ) / 2;
    ImpliedProb = diff( output.data$ImpliedProb );
    output.data = data.frame( Strike = Strike, ImpliedProb = ImpliedProb );
  };

  return( output.data );
};

###########################################################################################
# Routine: clean.cum.prob.data
#
# Try to fit implied prob data to a lognormal model, and discard points whose residuals
# are more than 'std.threshold' standard devations from the fitted lognormal.
###########################################################################################

clean.cum.prob.data <- function( 
  strikes, 
  probs,
  std.threshold = 2
) {
  formula = ImpliedProb ~ plnorm( Strike, meanlog, sdlog );
  start   = list( meanlog = log(20), sdlog = 1/2 );
  data    = data.frame( Strike = strikes, ImpliedProb = probs ); 

  fit = nls( formula, data, start );
  res = nlsResiduals( fit );
    
  bad.pts = which( abs( res$resi2[, "Standardized residuals" ] ) > std.threshold );
  if( length( bad.pts ) > 0 ) 
    data    = data[ -bad.pts, ];

  return( data );
};


###########################################################################################
# Routine: plot.implied.prob 
###########################################################################################

plot.implied.prob <- function( 
  ticker, 
  yearMonth, 
  cum = TRUE 
) {
  implied.prob.data = calc.implied.prob( ticker, yearMonth, cum = cum );
  spot = implied.prob.data$Spot;

  for( opt.type in c( "C", "P" ) ) {
    prob = implied.prob.data[[ opt.type ]]$Probs;
    strike   = implied.prob.data[[ opt.type ]]$Strikes;

    if( opt.type == "C" ) {
      plot( strike[ strike >= spot ], prob[ strike >= spot ], type="o", col="blue", cex=0.5,
                       xlim=c( -5, max( strike )*1.2), ylim=c(-0.1, 1.1 ) );
    } else {
      points( strike[ strike <= spot ], prob[ strike <= spot ], type="o", col="red", cex=0.5 );
    };
  };
};

###########################################################################################
# Routine: prepare.option.price.ts
###########################################################################################

proxy.option.price.ts <- function(
  Option.Symbol,
  BidAsk = 'Mid',
  start.date = today() %m-% years(2),
  end.date = last.biz.date( today() )
) {  
    info = parse.option.symbol( Option.Symbol );
    underlier.symbol = info$Symbol;
    Strike = info$Strike;
    Expiration.Date = info$Expiration.Date;
    Type = info$Type;

    # Get dividends
    div.ts = get.dividend.yield.ts( info$Symbol, start.date = start.date, end.date = end.date ) / 100;
    if( row.names( div.ts[1,] ) > start.date ) {
      div.ts = merge( timeSeries( div.ts[1], start.date, colnames(div.ts) ), div.ts  );
    };
    div.ts = fill.missing( div.ts, start.date = start.date, end.date = end.date, only.weekdays = TRUE );

    # Get implied vols
    imp.vol.ts = calc.option.implied.vol.ts( Option.Symbol = Option.Symbol, BidAsk = BidAsk, 
								start.date = start.date, end.date = end.date );
    if( row.names( imp.vol.ts[1,] ) > start.date ) {
      imp.vol.ts = merge( timeSeries( imp.vol.ts[1], start.date, colnames(imp.vol.ts) ), imp.vol.ts  );
    };
    imp.vol.ts = fill.missing( imp.vol.ts, start.date = start.date, end.date = end.date, only.weekdays = TRUE );

    # Get spots, and risk-free rates
    underlier.ts = fill.missing( get.time.series( info$Symbol, data.type = 'Close', start.date = start.date, end.date = end.date ), only.weekdays = TRUE );
    rf.ts = fill.missing( get.interest.rate( 'USD', n.months = 1, start.date = start.date, end.date = end.date ), only.weekdays = TRUE );
    dates = as.Date( row.names( underlier.ts ) );

    # Get the maturities for each of the pricing dates
    maturities = ( as.numeric( Expiration.Date ) - as.numeric( dates ) ) / 365.25;

    # Get the split-adjusted strike for each of the pricing dates
    split.ts = get.stock.split.ts( underlier.symbol, start.date = start.date, end.date = end.date );
    filled.split.ts = na.locf( insert.dates.ts( prod.ts( -1 + split.ts ), rownames( underlier.ts ) ) );
    splits = as.numeric( filled.split.ts[ dates, ] ) / as.numeric( filled.split.ts[ info$asOfDate ] );
    strikes = Strike * splits;

    prices = numeric(length(underlier.ts));
    for( i in 1:length(underlier.ts) ) {
      t = dates[i]; 

      spot = as.numeric( underlier.ts[t] );
      rfr = as.numeric( rf.ts[t] );
      div = as.numeric( div.ts[t] );
      imp.vol = as.numeric( imp.vol.ts[t] );
      if( maturities[i] > 0 ) {
        prices[i] = 1/splits[i] * calc.bs.price( Type = Type, Strike = strikes[i], Spot = spot, maturity = maturities[i], 
							vol = imp.vol, div = div, rf.rate = rfr );
      };
    };

    prices.ts = timeSeries( prices, dates, Option.Symbol );
    return( prices.ts );
};

###########################################################################################
# Routine: calc.option.implied.vol.ts
###########################################################################################

calc.option.implied.vol.ts <- function(
  Option.Symbol,
  BidAsk = 'Mid', 
  start.date = today() %m-% years(2),
  end.date = last.biz.date( today() )
) {
  info = parse.option.symbol( Option.Symbol );
  ED = info$Expiration.Date;
  sym.info = get.split.adj.option.symbol.ts( info$Short.Option.Symbol, asOfDate = info$asOfDate, start.date = start.date, end.date = ED );

  if( nrow( sym.info ) == 1 ) {
    vol.ts = calc.option.implied.vol.simple.ts( Option.Symbol, BidAsk = BidAsk, start.date = start.date, end.date = ED );
  } else {
    for( k in 1:nrow( sym.info ) ) {
      opt.sym = sym.info$Symbol[k];
      if( k == 1 ) {
        vol.ts = calc.option.implied.vol.simple.ts( opt.sym, BidAsk = BidAsk, start.date = start.date, end.date = sym.info$Date[k+1] - 1 );
        colnames( vol.ts ) = Option.Symbol;
      } else if( k < nrow( sym.info ) ) {
        new.ts = calc.option.implied.vol.simple.ts( opt.sym, BidAsk = BidAsk, start.date = sym.info$Date[k], end.date = sym.info$Date[k+1] - 1 );
        colnames( new.ts ) = Option.Symbol;
        if( is.empty( vol.ts ) ) {
          vol.ts = new.ts;
        } else {
          vol.ts = merge( vol.ts, new.ts );
        };
      } else {
        new.ts = calc.option.implied.vol.simple.ts( opt.sym, BidAsk = BidAsk, start.date = sym.info$Date[k], end.date = ED );
        colnames( new.ts ) = Option.Symbol;
        colnames( new.ts ) = Option.Symbol;
        if( is.empty( vol.ts ) ) {
          vol.ts = new.ts;
        } else {
          vol.ts = merge( vol.ts, new.ts );
        };
      };
    };
  }; 

  all.dates = as.Date( row.names( vol.ts ) );
  wkdays = all.dates[ wday(all.dates) %in% 2:6 ];

  implied.vol = ksmooth( as.numeric( all.dates ), as.numeric( vol.ts ), x.points = as.numeric(all.dates), bandwidth = 3 );
  imp.vol.ts = timeSeries( implied.vol$y, all.dates, Option.Symbol );
  imp.vol.ts = imp.vol.ts[ wkdays, ];

  imp.vol.ts = ts.range( imp.vol.ts, start.date = start.date, end.date = end.date );
  return(imp.vol.ts);
};

###########################################################################################
# Routine: calc.option.implied.vol.simple.ts
###########################################################################################

calc.option.implied.vol.simple.ts <- function(
  Option.Symbol,
  BidAsk = 'Mid',
  start.date = today() %m-% years(2),
  end.date = last.biz.date( today() )
) {
   nearest.symbol = get.closest.db.option.symbol( Option.Symbol, start.date = start.date, end.date = end.date );
   if( nearest.symbol != Option.Symbol ) {
     print( sprintf( 'Using implied vol for %s, as %s is not in the database.', nearest.symbol, Option.Symbol )  );
   };

   db.ts = get.db.option.ts( nearest.symbol, BidAsk = BidAsk, start.date = start.date, end.date = end.date );
   db.ts = db.ts[ !is.na( db.ts ), ];
   if( is.empty( db.ts ) ) {
     return( timeSeries() );
   };

   dates = as.Date( row.names( db.ts ) );

   info = parse.option.symbol( nearest.symbol );
   underlier.symbol = info$Symbol;
   Strike = info$Strike;
   Expiration.Date = info$Expiration.Date;
   Type = info$Type; 

   # Get dividends, spots, and risk-free rates
   div.ts = get.dividend.yield.ts( info$Symbol, start.date = start.date, end.date = end.date ) / 100;
   if( row.names( div.ts[1,] ) > start.date ) {
     div.ts = merge( timeSeries( div.ts[1], start.date, colnames(div.ts) ), div.ts  );
   }; 

   div.ts = fill.missing( div.ts, start.date = start.date, end.date = end.date );

   # Get spots, and risk-free rates
   underlier.ts = fill.missing( get.time.series( info$Symbol, data.type = 'Close', start.date = start.date, end.date = end.date ) );
   rf.ts = fill.missing( get.interest.rate( 'USD', n.months = 1, start.date = start.date, end.date = end.date ) );

   # Confine the time 
   div.yields = div.ts[ dates, ];
   spots = underlier.ts[ dates, ];
   rf.rates = rf.ts[ dates, ];

   maturities = ( as.numeric( Expiration.Date ) - as.numeric( dates ) ) / 365.25;

   vols = numeric(length(dates));
   for( t in 1:length(dates) ) {
     if( maturities[t] > 0 ) {
       vols[t] = calc.bs.vol( Type = Type, Strike = Strike, Spot = spots[t], maturity = maturities[t], price = db.ts[t], 
			div = div.yields[t], rf.rate = rf.rates[t] );
     };
   };

   vol.ts = fill.missing( timeSeries( vols, dates, 'Volatility' ) );

   all.dates = as.Date( row.names( vol.ts ) );
   wkdays = all.dates[ wday(all.dates) %in% 2:6 ];

   implied.vol = ksmooth( as.numeric( all.dates ), as.numeric( vol.ts ), x.points = as.numeric(all.dates), bandwidth = 3 );
   imp.vol.ts = timeSeries( implied.vol$y, all.dates, Option.Symbol );
   imp.vol.ts = imp.vol.ts[ wkdays, ];

   return( imp.vol.ts );
};


###########################################################################################
# Routine: calc.implied.vol
###########################################################################################

calc.implied.vol <- function( 
  ticker, 
  yearMonth, 
  divYield, 
  put.call = 'Split', 
  bid.ask  = "Mid",
  asOfDate     = last.option.date( ticker ),
  riskFreeRate = as.numeric( tail( get.interest.rate( 'USD', n.months = 1, start.date = asOfDate - weeks(1) ), 1 ) ),
  strike.range = c( 0, Inf )
) {
  spot = as.numeric( get.last.price( ticker ) );
  divYield = get.dividend.yield( ticker );

  opt.data  = get.option.data( ticker, yearMonth, asOfDate = asOfDate );

  optTypeNames = list( C = "call", P="put" );

  expiry = get.expiries( 'US Options', start.ym = yearMonth, end.ym = yearMonth );
  maturity = ( as.numeric( expiry ) - as.numeric( today() ) ) / 365.25;

  if( put.call == "All" || put.call == "Split" ) {
    opt.types = c( "P", "C" )
  } else if( put.call %in% c( "Put", 'put' ) ) {
    opt.types = "P" 
  } else if( put.call %in% c( "Call", 'call' ) ) {
    opt.types = "C"  
  } else {
    stop( paste( "Unksupported put.call type:", put.call ) )
  };

  imp.vol.data = data.frame();
  for( opt.type in opt.types ) {
    data = opt.data[ opt.data$Type == opt.type, ];

    strikes = data$Strike;
    if( bid.ask == 'Mid' ) {
      prices  = ( data$Bid + data$Ask ) / 2;
    } else {
      prices  = data[[ bid.ask ]];
    };
      
    openInt = data$OpenInt;

    imp.vol   = rep( NA, length( strikes ) );
    imp.vol.K = rep( NA, length( strikes ) );

    for( i in 1:length( strikes ) ) {
       if( put.call == 'Split' ) {
         if( ( opt.type == "C" & strikes[i] < spot ) || ( opt.type == "P" & strikes[i] > spot ) ) {
           next;
     	 };
       };
      
       try(
       {
	 result = calc.bs.vol( Type = opt.type, Strike = strikes[i], Spot = spot, maturity = maturity, price = prices[i], 
					rf.rate = riskFreeRate, div = divYield );

         if( class(result) != 'try-error' ) {
              imp.vol[i] = result;
         }
       }, silent = TRUE );
    };

    imp.vol.data = rbind( imp.vol.data, data.frame( Strike=strikes[!is.na(imp.vol)], ImpliedVol=imp.vol[ !is.na( imp.vol ) ], 
				OpenInt = data$OpenInt[ !is.na( imp.vol ) ] ) );
  };

  new.cols = data.frame( log_k = log( imp.vol.data$Strike / spot ), 
					ImpliedVariance = imp.vol.data$ImpliedVol*imp.vol.data$ImpliedVol * maturity);

  imp.vol.data = cbind( imp.vol.data, new.cols );

  imp.vol.data = imp.vol.data[ strike.range[1] * spot <= imp.vol.data$Strike & imp.vol.data$Strike <= strike.range[2]*spot, ];
  return( imp.vol.data );
};

###########################################################################################
# Routine: calc.SVI
#
# SVI (Stochastic Volatility Inspired) - implements Gatherals paper on parameterizing vol surface
###########################################################################################

calc.SVI <- function( 
  ticker,
  yearMonth,
  divYield, 
  put.call = 'All',
  bid.ask  = "Ask", 
  min.openInt = 1e-3, 
  strike.range = c( 0, Inf ), 
  asOfDate = last.option.date( ticker )
) {
  imp.vol = calc.implied.vol( ticker = ticker, yearMonth = yearMonth, put.call = put.call, bid.ask = bid.ask, 
					divYield = divYield, strike.range = strike.range, asOfDate = asOfDate );

  imp.vol = imp.vol[ order( imp.vol$Strike ), ];
  imp.vol = imp.vol[ imp.vol$OpenInt > min.openInt * sum( imp.vol$OpenInt ), ];

  formula = ImpliedVol ~ ( a + b * ( rho * ( log_k - m ) + sqrt( ( log_k - m ) * ( log_k - m ) + sigma * sigma ) ) )

  start = list( a = .04, b = 0.4, sigma = .1, rho = -0.4, m = 0 );
  fit = nls( formula, imp.vol, start, algorithm = "port" );

  data = cbind( imp.vol, data.frame( FittedVol = predict( fit ) ) );
  return( data );
};

###########################################################################################
# Routine: plot.SVI
###########################################################################################

plot.SVI <- function( 
  ticker,
  yearMonths,
  put.call = 'All',
  bid.ask  = "Ask",
  min.openInt = 1e-3,
  divYield = 0,
  strike.range = c( 0, Inf ), 
  plot.total.vol = FALSE, 
  type = "All", 
  asOfDate = last.option.date( ticker )
) {
  colors = c( "red", "blue", "black", "green", "magenta", "orange", "yellow" );

  for( i in 1:length( yearMonths ) ) {
    yearMonth = yearMonths[ i ];

    if( plot.total.vol == TRUE ) {
       expiry = get.expiries( 'US Options', start.ym = yearMonth, end.ym = yearMonth );
       maturity.factor = ( as.numeric( expiry ) - as.numeric( today() ) ) / 365.25;
    } else {
       maturity.factor = 1;
    };

    imp.vol = calc.SVI( ticker = ticker, yearMonth = yearMonth, put.call = put.call, bid.ask = bid.ask, 
				min.openInt = min.openInt, divYield = divYield, strike.range = strike.range, asOfDate = asOfDate );

    if( type == "All" || type == "ImpliedVol" ) {
      if( i == 1 ) {   
        plot( imp.vol$Strike, imp.vol$ImpliedVol * maturity.factor, col = colors[i] ); 
      } else {
        points( imp.vol$Strike, imp.vol$ImpliedVol * maturity.factor, col = colors[i] ); 
      };
    };
      
    if( type == "All" || type == "FittedVol" )
      lines( imp.vol$Strike, imp.vol$FittedVol * maturity.factor, col = colors[i] );
  };
};

###########################################################################################
# Routine: plot.implied.vol
###########################################################################################

plot.implied.vol <- function( 
  tickers,
  yearMonths,
  divYield,
  put.call = 'Split',
  bid.ask  = "Ask",
  strike.range = c( 0, Inf ),
  rescale.strikes = TRUE, 
  plot.total.vol  = FALSE, 
  asOfDate        = last.option.date( tickers )
) {
  colors=c( "blue", "red", "orange", "purple", "black", "green", "yellow", "pink", "magenta" )

  x.min = 0;
  x.max = 0;
  y.min = 1e5;
  y.max = 0;

  dt = list();
  labels = c();
  for( i in 1:length( tickers ) )
  {
    for( ym in 1:length( yearMonths ) ) 
    {
      yearMonth = yearMonths[ ym ];

      imp.vol = calc.implied.vol( tickers[i], yearMonth, put.call = put.call, bid.ask = bid.ask, 
							divYield = divYield, strike.range = strike.range );
      if( rescale.strikes == TRUE ) {
        spot = as.numeric( get.last.price( tickers[i] ) );
        imp.vol$Strike     = 100 * ( -1 + imp.vol$Strike / spot );
        imp.vol$ImpliedVol = 100 * imp.vol$ImpliedVol;
      };

      x.min = min( x.min, min( imp.vol$Strike ) );
      x.max = max( x.max, max( imp.vol$Strike ) );
      y.min = min( y.min, min( imp.vol$ImpliedVol ) );
      y.max = max( y.max, max( imp.vol$ImpliedVol ) );

      if( plot.total.vol == TRUE ) {
  	 expiry = get.expiries( 'US Options', start.ym = yearMonth, end.ym = yearMonth );
  	 maturity.factor = ( as.numeric( expiry ) - as.numeric( today() ) ) / 365.25;
      } else {
         maturity.factor = 1;
      };
 
      dt[[ length( dt ) + 1 ]] = list( ticker=tickers[i], Strike=imp.vol$Strike, ImpliedVol=imp.vol$ImpliedVol * maturity.factor );
      labels = c( labels, paste( tickers[i], yearMonth ) );
    };
  };

  for( i in 1:length( dt ) )
  {
    if( i == 1 ) {
      plot(   dt[[i]]$Strike, dt[[i]]$ImpliedVol, type="p", col=colors[i], main="Implied Vol vs Strike", pch = 1, 
                xlab="Strikes (as % of Spot)", ylab="Implied Volatility", xlim = c( x.min, x.max ), ylim = c( y.min, y.max ) );
    } else {
      points( dt[[i]]$Strike, dt[[i]]$ImpliedVol, type="p", col=colors[i], pch = 1);
    };
  };

  legend( "right", "top", legend=labels, fill = colors[1:length(labels) ] );
};

###########################################################################################
# Routine: plot.option.prices
###########################################################################################
plot.option.prices <- function( 
  ticker, 
  yearMonths,
  asOfDate = last.option.date( ticker ), 
  DB = MYSQL_DB_NAME
) {
  colors = c( "red", "blue", "black", "green", "magenta", "orange", "yellow" );

  for( i in 1:length( yearMonths ) ) {
    yearMonth = yearMonths[i];
    data  = get.option.data( ticker, yearMonth, asOfDate = asOfDate, DB = DB );
    calls = data[ data$Type == 'C', ];
    puts  = data[ data$Type == 'P', ];

    xlim = c( 0, max( data$Strike ) );
    ylim = c( 0, max( data$Ask ) );

    if( i == 1 ) {
      plot( calls$Strike, calls$Bid, col=colors[i], cex=0.5, pch = 1, xlim = xlim, ylim = ylim );
    } else { 
      points( calls$Strike, calls$Bid, col=colors[i], cex=0.5, pch = 1 );
    };

    riskFreeRate = 0.01;
    spot     = as.numeric( get.last.price( ticker ) );
    maturity = ( as.numeric( as.Date( sprintf( '%s-%s-18', round( yearMonth / 100 ), yearMonth %% 100 ) ) ) - as.numeric( today() ) ) / 365;
    Forward = spot * exp( maturity * riskFreeRate );
    cres = smooth.option.prices( calls$Strike, calls$Ask, opt.type = "C" , Forward = Forward );
    pres = smooth.option.prices( puts$Strike,  puts$Ask,  opt.type = "P",  Forward = Forward );
    lines( cres );
    lines( pres );

    points( calls$Strike, calls$Ask, col=colors[i], cex=0.5, pch = 2 );
    points( puts$Strike,  puts$Bid,  col=colors[i], cex=0.5, pch = 1 );
    points( puts$Strike,  puts$Ask,  col=colors[i], cex=0.5, pch = 2 );


    abline( a = spot, b = -1, lty = 2 );
    abline( a = -spot, b = 1, lty = 2 );
    abline( h = 0, lty = 2 );
  };
};

###########################################################################################
# Routine: option.prices.from.prob 
#
# This function calculates option prices from Cumulative probabilities, assuming the Spot is 1
###########################################################################################

option.prices.from.prob <- function(
  cum.probs,
  Strikes,
  type
) {
  Midpoints = ( Strikes[ 1:(length( Strikes ) - 1 ) ] + Strikes[ 2:length( Strikes ) ] ) / 2;
  probs = diff( cum.probs ) / diff( Strikes );

  if( type == "call" ) {
     C1 = Strikes * ( 1 - cum.probs );
     C2 = rev( c( 0, cumsum( rev( Midpoints * diff( cum.probs ) ) ) ) );

     prices = pmax( C2 - C1, 0 );
  } else if( type == "put" ) {
     P1 = Strikes * cum.probs;
     P2 = c( 0, cumsum( Midpoints * diff( cum.probs ) ) );

     prices = pmax( P1 - P2, 0 );
  };

  return( prices );
};

###########################################################################################
# Routine: calc.option.probs
#
# This function calculates cumulative probabilities from option prices
###########################################################################################

calc.option.probs <- function(
  Strikes, 
  prices, 
  type
) {
  Midpoints = ( Strikes[ 1:(length( Strikes ) - 1 ) ] + Strikes[ 2:length( Strikes ) ] ) / 2;
  cp = diff( prices ) / diff( Strikes );

  if( type == 'call' ) { 
    cp = cp + 1;
  };

  return( list( Midpoints = Midpoints, CumProbs = cp ) );
};


###########################################################################################
# Routine: smooth.option.prices
###########################################################################################

smooth.option.prices <- function( 
  Strikes, 
  Prices, 
  opt.type, 
  Forward
) {
  if( opt.type == "C" ) {
    lower.bound = function(x) pmax( rep( 0, length(x) ), -x + Forward );
    rasymp = Y ~ sqrt( X^2 + a^2 ) - X;
    lasymp = Y ~ sqrt( ( X - Forward )^2 + a^2 );
    start.vals = list( left = list( a = 2 ), right = list( a = 2 ) );
  } else if( opt.type == "P" ) {
    lower.bound = function(x) pmax( rep( 0, length(x) ),  x - Forward );
    lasymp = Y ~ a * X^2 + b * X;
    rasymp = Y ~ sqrt( ( X - Forward )^2 + a^2 );
    start.vals = list( left = list( a = 2, b = 2 ), right = list( a = 2 ) );
  };

  res = piecewise.smooth( Strikes, Prices, degree = 2, lower.bound = lower.bound, left.asymp = lasymp, right.asymp = rasymp, start.vals = start.vals );

  return( data.frame( Strike = res$x, Price = res$y ) );
};

###########################################################################################
# Routine: calc.option.greeks
###########################################################################################

calc.option.greeks <- function( 
  opt.type, 
  price = NA,
  vol = NA, 
  spot, 
  strike, 
  div, 
  rf.rate, 
  maturity
) {
  if( is.na(vol) && is.na(price) ) {
    stop( 'Either price or vol must be provided' );
  } else if( is.na(price) ) {
    price = calc.bs.price( Type = opt.type, Spot = spot, Strike = strike, div = div, rf.rate = rf.rate, maturity = maturity, vol = vol );
  };

  greeks = data.frame( ImpliedVol = NA, Delta = NA, Gamma = NA, Theta = NA, Vega = NA );

  try( {
    delta = calc.bs.greeks( greek.type = 'delta', Type = opt.type, price = price, Spot = spot, Strike = strike, div = div,
							rf.rate = rf.rate, maturity = maturity );
    gamma = calc.bs.greeks( greek.type = 'gamma', Type = opt.type, price = price, Spot = spot, Strike = strike, div = div,
							rf.rate = rf.rate, maturity = maturity );
    theta = calc.bs.greeks( greek.type = 'theta', Type = opt.type, price = price, Spot = spot, Strike = strike, div = div,
							rf.rate = rf.rate, maturity = maturity );
    vega  = calc.bs.greeks( greek.type = 'vega',  Type = opt.type, price = price, Spot = spot, Strike = strike, div = div,
							rf.rate = rf.rate, maturity = maturity );
    rho   = calc.bs.greeks( greek.type = 'rho',   Type = opt.type, price = price, Spot = spot, Strike = strike, div = div,
							rf.rate = rf.rate, maturity = maturity );

    greeks = data.frame( ImpliedVol = vol, Delta = delta, Gamma = gamma, Theta = theta, Vega = vega, Rho = rho );

  }, silent = TRUE );

  return( greeks );
};


