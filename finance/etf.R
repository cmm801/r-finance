
##########################################################################################################
# Routine: create.etf.proxies
#
# Given an ETF and its underlier, reconstruct what the past returns might have looked like using linear
# regression.  
##########################################################################################################

create.etf.proxies <- function( 
  etf.symbols, 
  start.date = today() %m-% years(30),
  end.date = last.biz.date( today() - 1 ), 
  nday.regression = 252, 
  recalculate = FALSE
) {
  if( is.empty( etf.symbols ) ) {
    return( NULL );
  }

  asset.info = get.etf.info( etf.symbols );

  all.symbols = setdiff( unique( c( etf.symbols, asset.info$Underlier.ID ) ), '' );

  all.ts <- get.time.series( all.symbols, start.date = start.date, end.date = end.date, rm.na = FALSE );

  proxies = list();
  for( i in 1:length( etf.symbols ) ) {
    etf.symbol = etf.symbols[i];
    ind.symbol = asset.info[ etf.symbol == asset.info$Symbol, 'Underlier.ID' ];

    # If no asset information is available or there is no underlier information, then just store the original time series
    if( !( etf.symbol %in% asset.info$Symbol ) || is.empty( ind.symbol ) ) {
      proxies[[i]] = all.ts[, name2label( etf.symbol ) ];
      next;
    };

    row = asset.info[ etf.symbols == etf.symbol, ];
    leverage = ifelse( is.empty( row$Leverage ), 1, row$Leverage );
   
    if( row$Underlier.Type == 'Futures' ) {
      proxy.ts <- prod.ts( returns( create.etf.futures.proxy( etf.symbol, start.date=start.date ), method='discrete' )*leverage );
      etf.ts <- ts.range( all.ts[,etf.symbol], start.date = first.date( proxy.ts ), end.date = last.date( proxy.ts ) );
    } else {
      etf.lab = name2label( etf.symbol );
      ind.lab = name2label( ind.symbol );

      common.dates = !is.na( all.ts[,etf.lab] ) & !is.na( all.ts[,ind.lab] ) & end.date - nday.regression <= rownames( all.ts );
      etf.ts = all.ts[ common.dates,];
      idx.ts = all.ts[ common.dates,];
      etf.rtns = returns( etf.ts[,etf.lab], method = 'discrete' );
      ind.rtns = returns( idx.ts[,ind.lab], method = 'discrete' );

      result <- lm( etf.rtns ~ ind.rtns );
  
      all.ind.rtns = returns( na.omit( all.ts[ , ind.lab ] ), method = 'discrete' );
      noise = sample( result$res, nrow( all.ind.rtns ), replace = TRUE );
      if( is.na( row$Include.Noise ) || row$Include.Noise == 0 ) {
        noise = noise * 0;
      };

      proxy.ts = prod.ts( result$coefficients[1] + result$coefficients[2] * all.ind.rtns + noise );
    };

    colnames( proxy.ts ) = etf.symbol;

    # Rescale the time series so that the last value is equal to the current spot of the equity
    rescale.factor = last( etf.ts[,etf.symbol] ) / last( proxy.ts[,etf.symbol] );
    proxy.ts <- proxy.ts * rescale.factor;
    proxies[[i]] = proxy.ts;
  };

  levels = merge.ts( proxies );
  return( levels );
};  

##########################################################################################################
# Routine: create.etf.futures.proxy
#
# Create a proxy for ETF futures indexes
##########################################################################################################

create.etf.futures.proxy <- function( 
  etf.symbol, 
  start.date = today() %m-% years( 30 ), 
  end.date   = last.biz.date( today() - 1 )
) {
  asset.info = get.etf.info( etf.symbol );

  if( asset.info$Underlier.ID == 'VXX' ) {
    ind.levels = roll.futures();
  } else {
   stop( paste( 'Unsupported Underlier ID for creating a futures-type etf proxy: ', etf.symbol ) );
  };
  
  colnames( ind.levels ) = etf.symbol;
  return( ind.levels );
};

##########################################################################################################
# Routine: regress.etf.on.underlier
#
# Look up the underlier for a given ETF
##########################################################################################################

regress.etf.on.underlier <- function( 
  etf, 
  nday.regression = 63,
  start.date = today() %m-% years(4)
) {
  data = get.etf.index.returns( etf, start.date = start.date );
  reg.ts = rolling.lm( Y = data[[1]], X = data[[2]], N = nday.regression );
  return( reg.ts );
}

##########################################################################################################
# Routine: find.etf.underlier
#
# Look up the underlier for a given ETF
##########################################################################################################

find.etf.underlier <- function( etf ) {
  info = get.etf.info( etf );
  underlier = ifelse( length( info ) > 0, info$Underlier.ID, '' );
  return( underlier ); 
}

#########################################################################################################
# Routine: get.etf.index.returns 
##########################################################################################################

get.etf.index.returns <- function( 
  etf, 
  index = find.etf.underlier(etf), 
  start.date = today() %m-% years(3), 
  end.date = last.biz.date()
) {
  if( is.character( etf ) & is.character( index ) ) {
    etf.prices = calc.price.return( etf,   start.date = start.date, end.date = end.date );
    ind.prices = calc.price.return( index, start.date = start.date, end.date = end.date );
    data = nanmerge( etf.prices, ind.prices );

    etf.returns <- returns( data[ ,which( colnames( data ) == etf ) ], method='discrete' );
    ind.returns <- returns( data[ ,which( colnames( data ) != etf ) ], method='discrete' );
  } else {
    etf.returns = etf;
    ind.returns = index;
  };

  return( list( ETF = etf.returns, Index = ind.returns ) );
};

#########################################################################################################
# Routine: plot.etf.regression 
#
# Plot the regression coefficients of an ETF on its underlier, along with the underlier's Std. Dev.
##########################################################################################################

plot.etf.regression <- function( 
  etf, 
  index, 
  start.date = today() %m-% years(3), 
  nday.regression = 252,  
  lag = TRUE
) {
  rtns = get.etf.index.returns( etf, index );
  etf.returns = rtns$ETF;
  ind.returns = rtns$Index;
  
  indStd = c();
  coefs  = c();
  coef.names = c( "Intercept", "Beta" );
  if( lag == TRUE )
    coef.names = c( coef.names, "LagETF", "LagBeta" );

  for( i in (nday.regression+1):length( etf.returns ) ) {
    etfRtn = etf.returns[ (i-nday.regression):i, ];
    indRtn = ind.returns[ (i-nday.regression):i, ];

    indStd = c( indStd, colSds( ind.returns[ (i-nday.regression):i, ] ) * sqrt( 252 ) );
   
    if( lag == TRUE ) {
      len <- length( etfRtn ); 
      reg <- lm( etfRtn[ 2:len ] ~indRtn[ 2:len ] + etfRtn[ 1:(len-1) ] + indRtn[ 1:(len-1) ] );
    } else {
      reg <- lm( etfRtn ~indRtn );
    };
 
    coefs <- rbind( coefs, reg$coefficients ); 
  };

  dates = rownames( etf.returns )[ (nday.regression+1):length( etf.returns ) ];

  par( mfrow=c( 3, 1 + ( lag == TRUE ) ) );
  
  coefs      = cbind( coefs, indStd );
  coef.names = c( coef.names, "Index_Std" ); 

  for( i in 1:length( coef.names ) ) {
    ts = timeSeries( coefs[, i ], dates, coef.names[i] );
    plot( ts, ylab = coef.names[i], type = 'l' );
    points( SMA( ts, 50 ), type = 'l', col = 'red' );
    points( SMA( ts, 200 ), type = 'l', col = 'blue' );
    abline( h = mean( ts ), col = 'red', lty = 2 );
  };

  colnames( coefs ) = coef.names;
  return( coefs )
}

##########################################################################################################
# Routine: calc.cum.prob.density.historical 
#
# Given ETF and index returns, calculate the conditional probability density of the ETF given the Index
##########################################################################################################

calc.cum.prob.density.historical <- function( 
  etf.rtns, 
  ind.rtns, 
  start.date = today() %m-% years(30), 
  nday.return=252
) {
  etf.levels <- cumulated( etf.rtns, method='discrete' );
  ind.levels <- cumulated( ind.rtns, method='discrete' );

  etf.lag.rtns <- -1 + etf.levels / lag( etf.levels, nday.return ); 
  ind.lag.rtns <- -1 + ind.levels / lag( ind.levels, nday.return ); 

  etf.lag.rtns <- etf.lag.rtns[ !is.na( etf.lag.rtns ), ];
  ind.lag.rtns <- ind.lag.rtns[ !is.na( ind.lag.rtns ), ];

  ordered.ind.rtns <- ind.lag.rtns[ order( ind.lag.rtns, decreasing=FALSE ) ];
  ordered.etf.rtns <- etf.lag.rtns[ order( ind.lag.rtns, decreasing=FALSE ) ];

  return( cbind( ordered.ind.rtns, ordered.etf.rtns ) )
};     

##########################################################################################################
# Routine: get.positions.from.sample 
#
# a function to help the bootstrap get vector indices from samples
##########################################################################################################

get.positions.from.sample = function( positions, block.length ) {
  new.positions = rep( NA, block.length * length( positions ) )
  for( i in 1:length( positions ) ) {
    new.positions[ (i-1) * block.length + 1:(block.length) ] = positions[i] + 0:(block.length-1);
  };
  return( new.positions );
};

##########################################################################################################
# Routine: calc.cum.prob.density.bootstrap 
#
##########################################################################################################

calc.cum.prob.density.bootstrap <- function(
  etf.rtns, 
  ind.rtns, 
  start.date = today() %m-% years(30), 
  nday.return=252, 
  block.length=5, 
  nsimulations=1e5
) {  
   
  samples <- sample( 1:(length( etf.rtns ) - block.length + 1 ), nsimulations * nday.return / block.length, replace=TRUE );
  samples <- matrix( samples, nrow=nsimulations );
  
  full.sample  <- apply( samples, 1, get.positions.from.sample, block.length=5 )
  sim.ind.rtns <- -1 + apply( full.sample, 2, function(x) prod( 1+ ind.rtns[x] ) )
  sim.etf.rtns <- -1 + apply( full.sample, 2, function(x) prod( 1+ etf.rtns[x] ) )

  ordered.sim.ind.rtns <- sim.ind.rtns[ order( sim.ind.rtns, decreasing=FALSE ) ];
  ordered.sim.etf.rtns <- sim.etf.rtns[ order( sim.ind.rtns, decreasing=FALSE ) ];

  return( cbind( ordered.sim.ind.rtns, ordered.sim.etf.rtns ) );
};

###########################################################################################################
# Routine: calc.cum.prob.density
#
# Indirector function that calls either the 'bootstrap' or 'historical' function
##########################################################################################################

calc.cum.prob.density <- function( 
  etf.rtns, 
  index.rtns, 
  method = 'bootstrap', 
  start.date= today() %m-% years(30), 
  nday.return=252
) {
  if( method == 'bootstrap' ) {
    return( calc.cum.prob.density.bootstrap( etf.rtns, index.rtns, start.date=start.date, nday.return=nday.return ) );
  } else {
    return( calc.cum.prob.density.historical( etf.rtns, index.rtns, start.date=start.date, nday.return=nday.return ) ); 
  };
};

##########################################################################################################
# Routine: calc.etf.cum.prob.density 
#
# Plot the regression coefficients of an ETF on its underlier, along with the underlier's Std. Dev.
##########################################################################################################

calc.etf.cum.prob.density <- function( 
  etf, 
  index, 
  method     	  = 'bootstrap', 
  start.date 	  = today() %m-% years(30), 
  nday.return     = 252, 
  nday.regression = 252 
) {
  rtns <- create.etf.proxies( etf, index, start.date = start.date, nday.regression=nday.regression );

  etf.rtns <- rtns[ ,which( colnames( rtns ) == etf ) ];
  ind.rtns <- rtns[ ,which( colnames( rtns ) != etf ) ];

  prob.density <- calc.cum.prob.density( etf.rtns, ind.rtns, method=method, start.date=start.date, nday.return=nday.return );
};

##########################################################################################################
# Routine: create.all.etf.proxies
#
# Creates all etf proxy return series at once
##########################################################################################################

create.all.etf.proxies <- function( 
  start.date       = today() %m-% years(30),
  nday.regression  = 252, 
  recalculate      = TRUE
) {
  asset.info = get.etf.info();   

  levels = list();
  for( i in 1:nrow( asset.info ) ) {
    row = asset.info[i, ];

    if( is.na( row$Underlier.ID ) || row$Underlier.ID == '' )
      next;

    print( row$Symbol );
    levels[[i]] = create.etf.proxies( row$ID, start.date = start.date, nday.regression = nday.regression, recalculate = recalculate ); 
  };
  
  ts = merge.ts( levels );
  return( ts );
};

###########################################################################################
# Routine: plot.etf.alpha
###########################################################################################

plot.etf.alpha <- function( 
  nday.regression = 63, 
  start.date = today() %m-% years(4) 
) {
  asset.info = get.etf.info();
  etf.info = asset.info[ '' != asset.info$Underlier.ID & asset.info$ID != asset.info$Underlier.ID,];

  for( i in 1:length( etf.info ) ) {
    data = get.etf.index.returns( etf.info[i,'ID'], start.date = start.date );
    
    reg.ts = rolling.lm( Y = data[[1]], X = data[[2]], N = N );
    ravg = rolling.avg( reg.ts[,'alpha'], N = 126 );
    print( sprintf( '%s %.4f %.4f', etf.info[i,'ID'], (1+tail(reg.ts$alpha,1))^252-1, (1+as.numeric(tail(ravg,1)))^252-1 ));
  };
};


###########################################################################################
# Routine: get.etf.data
###########################################################################################

get.etf.data <- function( long, short, start.date=Sys.Date()-3*365.25 ) {
  tickers <- c( long, short );
  ts <- get.time.series( tickers, start.date=start.date );
  return( ts );
};

###########################################################################################
# Routine: get.and.calc.pair.perf
#
# Given long and short ETF tickers, calculate the performance for a portfolio that rebalances 
# with equal weights of each ETF
###########################################################################################

get.and.calc.pair.perf <- function( long, short, start.date=Sys.Date()-3*365.25, rebal=20, margin=0 ) {
  etf.data <- get.etf.data( long, short, start.date=start.date );

  long_rtn  <- returns( etf.data[, long ],  method='discrete' );
  short_rtn <- returns( etf.data[, short ], method='discrete' );

  all_ts   <- calc.pair.perf( long_rtn, short_rtn, start.date=start.date, rebal=rebal, margin=margin );
  return( all_ts );
};

###########################################################################################
# Routine: calc.pair.perf
#
# Given formatted long and short return series, calculate the performance for a portfolio
# that shorts both of them, and rebalances periodically
###########################################################################################

calc.pair.perf <- function( long_rtn, short_rtn, start.date=Sys.Date()-3*365.25, rebal=20, margin=0 ) {
  cash_rtn  = long_rtn * 0;
 
  dates = rownames( long_rtn );

  long_mv  = c( -1, rep( 1, length( dates ) - 1 ) * NaN );
  short_mv = c( -1, rep( 1, length( dates ) - 1 ) * NaN );
  cash_mv  = c(  3, rep( 1, length( dates ) - 1 ) * NaN );
  total_mv = c(  1, rep( 1, length( dates ) - 1 ) * NaN );
  
  sd=1;
  for( dt in 2:length( dates ) ) {
    long_mv[dt]  = long_mv[  dt-1 ] * ( 1 + long_rtn[  dt - 1 ] );   
    short_mv[dt] = short_mv[ dt-1 ] * ( 1 + short_rtn[ dt - 1 ] );   
    cash_mv[dt]  = cash_mv[  dt-1 ] * ( 1 + cash_rtn[  dt - 1 ] );   
    
    total_mv[dt] = long_mv[  dt ] + short_mv[ dt ] + cash_mv[ dt ];

    if( dt - sd > rebal || abs( total_mv[dt]/long_mv[dt] ) < margin || abs( total_mv[dt]/short_mv[dt] ) < margin) {
      long_mv[dt]  = -total_mv[dt-1];
      short_mv[dt] = -total_mv[dt-1];
      cash_mv[dt]  = 3*total_mv[dt-1];
      sd = dt;
    };
  };

  long_ts  = timeSeries( long_mv,  dates, colnames( long_rtn )[1]  );
  short_ts = timeSeries( short_mv, dates, colnames( short_rtn )[1] );
  cash_ts  = timeSeries( cash_mv,  dates, "cash" );
  total_ts = timeSeries( total_mv, dates, "total" );

  all_ts   = merge( long_ts, merge( short_ts, merge( cash_ts, total_ts ) ) ); 
  return( all_ts );
};

###########################################################################################
# Routine: calc.etf.tracking.error
###########################################################################################

calc.etf.tracking.error <- function( 
  symbols = NULL,
  nday.regression = 126, 
  nday.movavg = 252 
) {
  asset.info = get.etf.info();
  if( is.empty( symbols ) ) {
    symbols = asset.info[ asset.info$ID != asset.info$Underlier.ID & asset.info$Underlier.ID != '', 'ID'];
  };

  alpha.list = c();
  beta.list = c();
  for( etf in symbols ) {
    reg = regress.etf.on.underlier( etf, nday.regression = nday.regression )
    alpha = as.numeric( tail( ( 1 + rolling.avg( reg[,1], nday.movavg ) )^252-1, 1 ) );
    alpha.list = c( alpha.list, alpha );
    beta.list = c( beta.list, as.numeric( tail( reg[,2], 1 ) ) );
  }

  output = data.frame( Symbol = symbols, Alpha = alpha.list, Beta = beta.list );
  return( output );
};

###########################################################################################
# Routine: calc.tracking.error.betas
###########################################################################################

calc.tracking.error.betas <- function( 
  symbols, 
  factor.list,
  freq = 'D',
  start.date = today() %m-% years(50), 
  end.date = last.biz.date()
) {
  options(stringsAsFactors = FALSE);
  asset.info = get.etf.info();

  etf.list = asset.info[ asset.info$ID != asset.info$Underlier.ID & asset.info$Underlier.ID != '', 'ID'];

  factor.ts = get.factor.ts( factor.list, freq = freq, start.date = start.date, end.date = end.date );
  risk.free.rate = get.factor.ts( 'RF', freq = freq, start.date = start.date, end.date = end.date );

  output = data.frame();
  for( etf in etf.list ) {
    if( etf %in% symbols ) {
         
      rtns = get.etf.index.returns( etf, start.date = start.date, end.date = end.date );
      etf.reg = lm( rtns$ETF ~ rtns$Index );
  
      resid = timeSeries( etf.reg$residuals );
      if( freq == 'M' ) {
        resid = daily2monthly.rtns( resid );
      };
  
      factor.reg = regress.asset.factor( factor.ts, resid, risk.free.rate = risk.free.rate, start.date = start.date );
      row.names( factor.reg ) = NULL;

      new.output = cbind( data.frame( Symbol = etf, Frequency = freq ), factor.reg );
      output = rbind( output, new.output );
    };
  }

  return( output );
};

