
###########################################################################################
# Routine: calc.pf.vol
#
# Calculate the portfolio volatility using one of several methods, including Historical,
# Intraday, and Implied.  If option symbols are included, then they are replaced by their
# Underliers for the calculation, and the weights are scaled by the 'option beta'.
###########################################################################################

calc.pf.vol <- function(
  symbols, 
  weights, 
  method = 'Historical', 
  freq = ifelse( method != 'Intraday', 'D', '15m' ), 
  start.date = last.biz.date() %m-% years(10), 
  end.date = last.biz.date()
) {
  # Replace Option Symbols with their underliers, scaled by the 'Option Beta'
  underlier.weights = get.portfolio.exposures( weights, symbols );
  underlier.symbols = names( underlier.weights );

  covar = calc.pf.cov( underlier.symbols, method = method, freq = freq, start.date = start.date, end.date = end.date ); 

  # Using the adjusted underlier weight vector and the covariance matrix of the underliers, calculate the portfolio volatility
  pf.vol = sqrt( underlier.weights %*% covar %*% underlier.weights );

  return( pf.vol );
};

###########################################################################################
# Routine: calc.pf.cov
#
# Calculate the portfolio volatility using one of several methods, including Historical,
# Intraday, and Implied.  If option symbols are included, then they are replaced by their
# Underliers for the calculation, and the weights are scaled by the 'option beta'.
###########################################################################################

calc.pf.cov <- function(
  underlier.symbols,
  method = 'Historical',
  freq = ifelse( method != 'Intraday', 'D', '15m' ),
  start.date = last.biz.date() %m-% years(10),
  end.date = last.biz.date()
) {
  if( method == 'Historical' ) {
    covar = calc.historical.cov( underlier.symbols, freq = freq, start.date = start.date, end.date = end.date );
  } else if( method == 'Implied' ) {
    covar = calc.implied.cov( underlier.symbols, start.date = start.date, end.date = end.date );
  } else if( method == 'Intraday' ) {
    covar = calc.intraday.cov( underlier.symbols, freq = freq, start.date = start.date, end.date = end.date );
  } else {
    # Else assume the string passed in is the name of a factor list
    covar = calc.asset.cov( underlier.symbols, factor.list = method, freq = freq, start.date=start.date, end.date=end.date );
  };

  colnames( covar ) = underlier.symbols;
  rownames( covar ) = underlier.symbols;

  return( covar );
};



###########################################################################################
# Routine: calc.historical.cov
###########################################################################################

calc.historical.cov <- function( 
  symbols,
  freq = 'D', 
  start.date = last.biz.date() %m-% years(10),
  end.date = last.biz.date()
) {
  daily.ts = get.time.series( symbols, start.date = start.date, end.date = end.date, rm.na = FALSE, use.proxy = TRUE );

  ts = adjust.ts.freq( daily.ts, freq = freq ); 
  rtns = ri( ts );
 
  freq.adj = get.freq.adj( freq );
  covar = cov( matrix( rtns, ncol = ncol( rtns ) ), use = 'complete.obs' ) * freq.adj;
  
  return( covar );
};

###########################################################################################
# Routine: calc.intraday.cov
###########################################################################################

calc.intraday.cov <- function(
  symbols,
  freq = '15m',
  start.date = last.biz.date() %m-% years(10),
  end.date = last.biz.date()
) {
  # Get price levels
  intraday.ts = get.rt.data( symbols, start.date = start.date, end.date = end.date, rm.na = FALSE );
  filled.ts = na.locf( intraday.ts );

  # Find the locations of prices at the specified Frequency
  minutes = as.numeric( gsub( 'm', '', freq ) ); 
  inds = seq( 1, nrow( filled.ts ), minutes );

  # Calculate the returns at the specified frequency
  ts = filled.ts[ inds, ];
  rtns = ri( ts );

  # Add 0's for missing series, along with a warning message
  missing.symbols = setdiff( symbols, colnames(rtns) );
  missing.rtns = timeSeries( matrix(0, ncol = length(missing.symbols), nrow = length(rownames(rtns) ) ), rownames(rtns), units = missing.symbols );
  rtns = nanmerge( rtns, missing.rtns )[,symbols];
  print( sprintf( 'WARNING: missing Intraday returns for %s', paste( missing.symbols, collapse = ', ' ) ) );

  freq.adj = 6.5 * 60 * 252 / minutes;
  covar = cov( matrix( rtns, ncol = ncol( rtns ) ), use = 'complete.obs' ) * freq.adj;

  return( covar );
};


###########################################################################################
# Routine: calc.implied.cov
###########################################################################################

calc.implied.cov <- function(
  symbols,
  start.date = last.biz.date() %m-% years(10),
  end.date = last.biz.date()
) {
  hist.cov = calc.historical.cov( symbols, freq = 'D', start.date = start.date, end.date = end.date );
  hist.std = sqrt( diag( hist.cov ) );
  hist.cor = cov2cor( hist.cov );

  expiration.date = today() %m+% months(1);

  yearMonth = 100*year(expiration.date) + month( expiration.date);

  std = hist.std;

  db.option.symbols = last.db.date( symbols, Table = 'YahooOptions' )$Symbol;
  for( i in 1:length( symbols ) ) {
    symbol = symbols[i];
   
    if( !(symbol %in% db.option.symbols ) ) {
      next;
    }; 
    
    try(
    { 
      divYield = get.dividend.yield( symbol );

      vols = calc.implied.vol( symbol, yearMonth = yearMonth, bid.ask = 'Mid', strike.range = c( 0.9, 1.1 ), put.call = 'Split',
                                        divYield = divYield );

      impliedVol = sum( vols$ImpliedVol * vols$OpenInt ) / sum( vols$OpenInt );
      std[i] = impliedVol; 

    }, silent = TRUE );
  };

  covar = diag( std ) %*% hist.cor %*% diag( std );
  return( covar );
};

###########################################################################################
# Routine: proxy.from.factors
#
# Use the factor returns to create a proxy time series for a given set of equity/ETF Symbols.
# If 'totalReturn' is TRUE, then add the risk-free rate back to all excess returns before
# returning the final results.
###########################################################################################

proxy.from.factors <- function(
  Symbols,
  factor.list = 'FF3',
  start.date = last.biz.date() %m-% years(50),
  end.date = last.biz.date(),
  freq = 'D', 
  totalReturn = TRUE
) {
  factor.names = get.factor.names( factor.list );
  excess.rtns = na.omit( get.factor.ts( c( factor.names, 'RF' ), start.date = start.date, end.date = end.date ) );
  factor.rtns = excess.rtns[, factor.names ];
  betas = get.beta( Symbols, factor.list = factor.list, freq = freq );
  rtns = timeSeries( as.matrix( factor.rtns ) %*% t( betas ) );

  if( totalReturn ) {
    for( i in 1:length( Symbols ) ) {
      rtns[,i] = rtns[,i] + excess.rtns[,'RF']; 
    };
  };

  return( rtns );
};

###########################################################################################
# Routine: simulate.from.factors
###########################################################################################

simulate.from.factors <- function(
  symbols,
  factor.list = 'FF3', 
  start.date = last.biz.date() %m-% years(50),
  end.date = last.biz.date(), 
  freq = 'D', 
  nperiods = 21, 
  block.size = 5, 
  num.sims = 1e4,  
  noise.type = 'normal'
) {
  symbol.list = get.asset.class.list( symbols );

  if( !is.empty( symbol.list$Option ) ) {
    option.info = parse.option.symbols( symbol.list$Option ); 
  } else {
    option.info = NULL;
  };

  if( !is.empty( symbol.list$CurrencyForward ) ) {
    fx.fwd.info = parse.fx.forward.symbols( symbol.list$CurrencyForward ); 
  } else {
    fx.fwd.info = NULL;
  };

  underliers = unique( c( symbol.list$Equity, symbol.list$Index, symbol.list$Cash, symbol.list$CurrencyPair, 
							option.info$Symbol, fx.fwd.info$Currency.Cross ) );

  proxy.rtns = proxy.from.factors( underliers, factor.list = factor.list, start.date = start.date, end.date = end.date, freq = freq );
  proxy.levels = prod.ts( proxy.rtns );
  bootstrap.rtns = simulate.returns( proxy.levels, num.sims = num.sims, ndays = nperiods, block.size = block.size, method = 'Block' );

  covar = calc.asset.cov( underliers, factor.list = factor.list, freq = freq ) + diag( 1e-10, length( underliers ) );
  correl = cov2cor( covar );

  if( noise.type == 'normal' ) {
    std = get.beta( underliers, factor.list = factor.list, freq = freq, return.all = TRUE )$idioStd;
    noise = chol.generate( num.sims, correl = correl, std = std );
  } else if( noise.type == 't' ) {
    info = get.beta( underliers, factor.list = factor.list, freq = freq, return.all = TRUE );
    noise = chol.generate( num.sims, mu, scale = info$Scale, df = info$df, ncp = info$ncp, correl = correl );
  };

  sim.rtns = t( bootstrap.rtns ) + noise * sqrt( nperiods );
  colnames( sim.rtns ) = underliers;  

  # Simulate options from underliers
  if( !is.empty( symbol.list$Option ) ) {
    option.rtns = simulate.from.factors.options( symbol.list$Option, sim.rtns = sim.rtns );
  } else {
    option.rtns = NULL;
  };
 
  # Simulate FX Forwards from underliers
  if( !is.empty( symbol.list$CurrencyForward ) ) {
    fx.fwd.rtns = simulate.from.factors.fx.forwards( symbol.list$CurrencyForward, sim.rtns = sim.rtns );
  } else {
    fx.fwd.rtns = NULL;
  };

  # Merge asset class returns, and discard any unneeded Underlier returns
  all.rtns = cbind( sim.rtns, cbind( option.rtns, fx.fwd.rtns ) )[, symbols ];
  return( all.rtns );
};

###########################################################################################
# Routine: simulate.from.factors.options
###########################################################################################

simulate.from.factors.options <- function(
  option.symbols,
  factor.list = 'FF3',
  start.date = last.biz.date() %m-% years(50),
  asOfDate = last.biz.date(),
  freq = 'D',
  nperiods = 21,
  block.size = 5,
  num.sims = 1e4, 
  sim.rtns = NULL
) {
  all.info = parse.option.symbols( option.symbols );
  underliers = unique( all.info$Symbol );

  if( is.null( sim.rtns ) ) {
    underlier.rtns = simulate.from.factors( symbols = underliers, factor.list = factor.list, start.date = start.date, 
			end.date = asOfDate, freq = freq, nperiods = nperiods, block.size = block.size, num.sims = num.sims );
  } else {
    underlier.rtns = sim.rtns;
  };

  # Change the spot, and change the rf.rate to be specific to the denominated currency
  spots = get.last.price( underliers );  # This should be changed to get the price on the asOfDate
  rf.rate = last( get.interest.rate( 'USD', n.months = 1, start.date = asOfDate %m-% months(1) ) );

  option.sims = list();
  for( i in 1:length( option.symbols ) ) {
    info = all.info[i,];
    Spot = spots[ names(spots) == info$Symbol ];
    Strike = info$Strike;

    opt.type = ifelse( info$Type == 'C', 'call', 'put' );
    yearMonth = date2yearMonth( info$Expiration.Date );
    divYield = get.dividend.yield( info$Symbol );
    maturity = ( as.numeric( as.Date( info$Expiration.Date ) ) - as.numeric( as.Date( asOfDate ) ) ) / 365.25;

    vol.info = calc.implied.vol( info$Symbol, yearMonth, opt.type, bid.ask = 'Mid', divYield = divYield, riskFreeRate = rf.rate,
                                                                strike.range = c(.9, 1.1 ) * Strike / Spot );
    if( nrow( vol.info ) != 0 ) {
      impliedVol = mean( vol.info$ImpliedVol );
    } else {
        vol.info = calc.implied.vol( info$Symbol, yearMonth, put.call = 'Split', bid.ask = 'Mid', divYield = divYield, 
					riskFreeRate = rf.rate, strike.range = c( 0, Inf ) );
        # Linearly interpolate the implied volatility, and use the vol of the closest strike if outside of the range
  	impliedVol = approx( vol.info$Strike, vol.info$ImpliedVol, Strike, rule = 2 )$y;
    };

    initial.price = calc.bs.price( info$Type, Strike = info$Strike, Spot = Spot, maturity = maturity,
                                                                        vol = impliedVol, div = divYield, rf.rate = rf.rate );

    final.prices = calc.bs.price( info$Type, Strike = info$Strike, Spot = Spot * ( 1 + underlier.rtns[,info$Symbol ] ), 
                                maturity = pmax( maturity - nperiods/252, 0 ), vol = impliedVol, div = divYield, rf.rate = rf.rate );

    option.sims[[i]] = final.prices / initial.price - 1;
  };

  option.rtns = matrix( merge.ts( option.sims ), ncol = length( option.symbols ) );
  colnames( option.rtns ) = option.symbols;
  return( option.rtns );
};

###########################################################################################
# Routine: simulate.from.factors.fx.forwards
###########################################################################################

simulate.from.factors.fx.forwards <- function(
  fx.fwd.symbols,
  factor.list = 'FF3',
  start.date = last.biz.date() %m-% years(50),
  asOfDate = last.biz.date(),
  freq = 'D',
  nperiods = 21,
  block.size = 5,
  num.sims = 1e4,
  sim.rtns = NULL
) {
  ###############################################################################################
  ## WARNING - cannot handle case where Settlement Date is less than nperiods from asOfDate    ##
  ###############################################################################################

  all.info = parse.fx.forward.symbols( fx.fwd.symbols );
  underliers = unique( all.info$Currency.Cross );

  if( is.null( sim.rtns ) ) {
    underlier.rtns = simulate.from.factors( symbols = underliers, factor.list = factor.list, start.date = start.date,
                        end.date = asOfDate, freq = freq, nperiods = nperiods, block.size = block.size, num.sims = num.sims );
  } else {
    underlier.rtns = sim.rtns;
  };

  # Get the last exchange rate available at the asOfDate. 
  # Include the last prior business date, in case the asOfDate is not a business date.
  spots = tail( get.exchange.rates( underliers, start.date = asOfDate %m-% months(1), end.date = asOfDate ), 1 );

  BidAsk = 'Mid';
  fx.fwd.sims = list();
  for( i in 1:length( fx.fwd.symbols ) ) {
    info = all.info[i,];
    underlier = info$Currency.Cross
    sim.fut.rates = ( 1 + sim.rtns[,underlier] ) * as.numeric( spots[,underlier] );
  
    forward.dates = c( DateRuleApply( asOfDate, sprintf( '%db', nperiods ) ), info$Settlement.Date );
    fwd.points = get.forward.points( underlier, forward.dates = forward.dates, pricing.date = asOfDate, BidAsk = BidAsk );
    fwd.points = fwd.points[ order( as.Date( fwd.points$Forward.Date ) ), ];
    net.fwd.points = fwd.points[2,BidAsk] - fwd.points[1,BidAsk];
    fx.rescale = get.fx.forward.points.rescale.factor( underlier );

    fx.fwd.sims[[i]] = info$Settlement.Rate - sim.fut.rates + net.fwd.points/fx.rescale;
  };

  fx.fwd.rtns = matrix( merge.ts( fx.fwd.sims ), ncol = length( fx.fwd.symbols ) );
  colnames( fx.fwd.rtns ) = fx.fwd.symbols;
  return( fx.fwd.rtns );
};

###########################################################################################
# Routine: simulate.pf.margin.equity.from.factors
#
# Simulate portfolio margin equity ratio from factors
###########################################################################################

simulate.pf.margin.equity.from.factors <- function(
  symbols,
  weights,
  factor.list = 'FF3',
  start.date = last.biz.date() %m-% years(50),
  asOfDate = last.biz.date(),
  freq = 'D',
  nperiods = 21,
  block.size = 5,
  num.sims = 1e4
) {
  sim.rtns = simulate.from.factors( symbols, factor.list, nperiods = nperiods, block.size = block.size,
                                                                                        num.sims = num.sims, freq = freq );
  mv = t( apply( sim.rtns, 1, function(x) (1+x)*weights ) );
  margin = calc.margin.equity.ratio( weights, symbols );

  return( margin );  
};


###########################################################################################
# Routine: simulate.pf.from.factors
#
# Simulate portfolio performance from factors
###########################################################################################

simulate.pf.from.factors <- function(
  symbols,
  weights,
  factor.list = 'FF3', 
  start.date = last.biz.date() %m-% years(50),
  asOfDate = last.biz.date(), 
  freq = 'D', 
  nperiods = 21, 
  block.size = 5, 
  num.sims = 1e4
) {
  sim.rtns = simulate.from.factors( symbols, factor.list, nperiods = nperiods, block.size = block.size, 
											num.sims = num.sims, freq = freq );
  pf.rtns =  sim.rtns %*% matrix( weights, ncol = 1 );
  return( pf.rtns );
};

###########################################################################################
# Routine: calc.pf.VaR
#
# Calculate the portfolio VaR.
###########################################################################################

calc.pf.VaR <- function(
  symbols,
  weights,
  factor.list = 'FF3', 
  start.date = last.biz.date() %m-% years(50),
  asOfDate = last.biz.date(), 
  freq = 'D', 
  VaR.pct = .05,
  nperiods = 21, 
  block.size = 5, 
  num.sims = 1e4
) {
  pf.rtns = simulate.pf.from.factors( symbols, weights, factor.list = factor.list, nperiods = nperiods, block.size = block.size,
                                                                                        num.sims = num.sims, freq = freq );

  VaR = quantile( pf.rtns, VaR.pct );
  return( VaR );
};


