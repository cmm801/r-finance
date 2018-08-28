
library( 'R2HTML' );

###########################################################################################
# Routine: my.pf.stats
#
# Print a variety of performance and risk statistics, and produce some plots
###########################################################################################

my.pf.stats <- function( 
  asOfDate = last.biz.date( today() ), 
  output.pathname = ''
) {
  # Define 'Trade' object to represent the portfolio transactions
  pf_trade <- initialize.trade( trade = '' );

  # Get Time-Weighted portfolio performance time series
  pf.ts = TotalReturn( pf_trade );

  # Construct Benchmark time series, and compare performance stats
  bm.rtns = ri( get.time.series( c( 'SPY', 'IEI' ), start.date = StartDate(pf_trade), end.date = asOfDate ) );
  benchmarks = prod.ts( merge.ts( list( bm.rtns[,'SPY'], 0.6*bm.rtns[,'SPY'] + 0.4*bm.rtns[,'IEI'], 
 							              	2*( 0.6*bm.rtns[,'SPY'] + 0.4*bm.rtns[,'IEI'] ) ) ) );
  colnames( benchmarks ) = c( 'SPXTR', '60/40 Eq/Bond', '2x 60/40' );
  bm.stats = get.ts.stats( nanmerge( pf.ts, benchmarks ) );
  print.portfolio.html( bm.stats, pathname = output.pathname, filename = 'benchmark_stats.html' );

  # PnL statistics
  pnl.stats = PnLSummary( pf_trade );
  print.portfolio.html( pnl.stats, pathname = output.pathname, filename = 'pnl_stats.html' );

  # Initialize Portfolio object
  pf = Portfolio( pf_trade );
  pf = setOptionInfo( pf );
  pf = setSimulation( pf );

  # Summary
  pf.summary = get.pf.summary( pf, format = TRUE );
  print.portfolio.html( pf.summary, pathname = output.pathname, filename = 'pf_summary.html' );

  # Risk Attribution
  risk.summary = TradeRiskSummary(pf);
  print.portfolio.html( risk.summary, pathname = output.pathname, filename = 'risk_summary.html' );

  # Risk Statistics
  risk.stats = my.pf.risk.stats( pf );
  print.portfolio.html( risk.stats, pathname = output.pathname, filename = 'risk_stats.html' );

  # 10 Worst Historical Draw-downs
  drawdowns = HistoricalDrawdowns( pf );
  print.portfolio.html( drawdowns[1:20,], pathname = output.pathname, filename = 'drawdowns.html' );

  # Individual Trade Performance
  trade.stats = my.trade.performance( pf_trade, asOfDate = asOfDate, Format = TRUE );
  print.portfolio.html( trade.stats, pathname = output.pathname, filename = 'trade_stats.html' );

  # Plot performance time series
  if( nchar(output.pathname) != 0 ) {

    # Plot portfolio total return
    pf.plot.filename = sprintf( '%s/pf_total_return.jpg', output.pathname );
    jpeg( pf.plot.filename );
    plot.pf.total.return( pf_trade );

    # Plot portfolio draw downs
    pf.plot.filename = sprintf( '%s/pf_drawdown.jpg', output.pathname );
    jpeg( pf.plot.filename );
    plot.pf.drawdown( pf_trade );

    # Plot portfolio beta
    pf.plot.filename = sprintf( '%s/pf_beta.jpg', output.pathname );
    jpeg( pf.plot.filename );
    plot.pf.beta( pf_trade );

    # Plot portfolio margin equity ratio
    pf.plot.filename = sprintf( '%s/pf_margin_equity_ratio.jpg', output.pathname );
    jpeg( pf.plot.filename );
    plot.pf.margin.equity.ratio( pf_trade );
    dev.off();

  } else {
    plot.pf.stats( pf_trade );
  };
};

###########################################################################################
# Routine: print.pf.summary
#
# Print info summarizing portfolio positions
###########################################################################################

print.pf.summary <- function(
  pf = NULL,
  asOfDate = last.biz.date( today() ),
  output.pathname = ''
) {
  # Define 'Trade' object to represent the portfolio transactions
  if( is.null( pf ) ) {
    pf <- get.portfolio( asOfDate );
  };

  by.cols = c( 'ExposureCurrency', 'Schema.Level.2', 'Schema.Level.3', 'Schema.Level.4' );
  for( by.col in by.cols ) {
    summarize.pf.cols( pf, by.col = by.col, agg.col = 'MarketValue', output.pathname = output.pathname );
    summarize.pf.cols( pf, by.col = by.col, agg.col = 'NotionalMktVal', output.pathname = output.pathname );
    summarize.pf.cols( pf, by.col = by.col, agg.col = 'MV.Wtd.Betas', output.pathname = output.pathname );
    summarize.pf.cols( pf, by.col = by.col, agg.col = 'Notional.Wtd.Betas', output.pathname = output.pathname );
    summarize.pf.cols( pf, by.col = by.col, agg.col = 'RiskAttribution', output.pathname = output.pathname, 
							format.type = 'double1', add.pct.col = FALSE );
  };
};

###########################################################################################
# Routine: summarize.pf.cols
#
# Print info summarizing portfolio positions
###########################################################################################

summarize.pf.cols <- function(
  pf = NULL,
  by.col = '', 
  agg.col = '', 
  asOfDate = last.biz.date( today() ),
  output.pathname = '', 
  format.type = 'MarketValue', 
  add.pct.col = TRUE
) {
  # Define 'Trade' object to represent the portfolio transactions
  if( is.null( pf ) ) {
    pf <- get.portfolio( asOfDate );
  };

  # Get summary info
  summary.df = Summary( pf );

  # Get the subset data frame
  df = summary.df[ c( 'Symbol', by.col, agg.col ) ];
  rownames(df) = NULL;

  if( add.pct.col ) {
    # Add a percent column for the quantity
    pct = df[ agg.col ] / sum( df[, agg.col ] );
    pct.col = paste( agg.col, '%', sep = '.' );
    colnames( pct ) = pct.col;
    df = cbind( df, pct );
    cols.to.keep = c( agg.col, pct.col );
  } else {
    cols.to.keep = agg.col;
  };
  
  # Aggregate the summary data frame
  agg.method = sum;
  agg.df = aggregate.df( df, by.cols = by.col, agg.methods = agg.method, cols.to.keep = cols.to.keep );

  if( add.pct.col ) {
    # Format the data frames
    fmt.df = format.df( df, format.pars = list( 'char', 'char', format.type, 'Percent' ) );
    agg.fmt.df = format.df( agg.df, format.pars = list( 'char', format.type, 'Percent' ) );
    tot.row = data.frame( 'Total', 'Total', sum( df[,agg.col] ), 1 );
  } else {
    # Format the data frames
    fmt.df = format.df( df, format.pars = list( 'char', 'char', format.type ) );
    agg.fmt.df = format.df( agg.df, format.pars = list( 'char', format.type ) );
    tot.row = data.frame( 'Total', 'Total', sum( df[,agg.col] ) );
  };

  # Add totals rows to both data frames
  colnames( tot.row ) = colnames( df ); 
  df = rbind( df, tot.row );
  agg.df = rbind( agg.df, tot.row[,-1]);

  # Print results or write them to a file
  filename = sprintf( '%s_by_%s.html', agg.col, by.col );
  filename.agg = sprintf( '%s_by_%s_agg.html', agg.col, by.col );
  print.portfolio.html( fmt.df, pathname = output.pathname, filename = filename );
  print.portfolio.html( agg.fmt.df, pathname = output.pathname, filename = filename.agg );
};

###########################################################################################
# Routine: print.portfolio.html
#
# Print stats, or save as an html file
###########################################################################################

print.portfolio.html <- function( data, pathname = '', filename = '' ) {
  if( nchar( pathname ) != 0 ) {
    file = sprintf( '%s/%s', pathname, filename );
    HTML( data, file = file, append = FALSE );
    HTML( sprintf( "<br><b>Last Updated:</b> %s", Sys.Date() ), file=file );
  } else {
    print( data );
  };
};


###########################################################################################
# Routine: compare.pf.performance
#
# Plot monthly portfolio returns and volatility, and compare to a benchmark
###########################################################################################

compare.pf.performance <- function( 
  pf_trade = NULL, 
  ticker = '^GSPC' 
) {
  ts = get.time.series( ticker );

  if( is.null( pf_trade ) ) {
    pf_trade = initialize.trade();  
  };
 
  my.ts = TotalReturn( pf_trade );  
  colnames( my.ts ) = 'Portfolio';

  all.ts = nanmerge( my.ts, ts );
 
  month.end = get.month.ends( last.date(ts), last.date(ts) );
  vol = daily2monthly( rolling.sd( all.ts, N = 21 ) );
  rtns = ri( daily2monthly( fill.missing( all.ts, end.date = month.end ) ) );

  rtn.data = t( as.matrix( rtns ) );
  vol.data = t( as.matrix( vol ) );

  rtn.dates = unlist( lapply( as.Date( rownames( rtns ) ), function(x) sprintf( '%s-%s', month.abb[ month(x) ], year(x) %% 100 ) ) );
  vol.dates = unlist( lapply( as.Date( rownames( vol  ) ), function(x) sprintf( '%s-%s', month.abb[ month(x) ], year(x) %% 100 ) ) );

  colnames( rtn.data ) = rtn.dates;
  colnames( vol.data ) = vol.dates;

  par( mfrow = c(2,1 ));

  barplot( rtn.data, beside = TRUE, col = c( 'red', 'blue' ) );
  legend( 'bottomleft', colnames( rtns ), fill = c( 'red', 'blue' ) );
  title( 'Returns' );
  barplot( vol.data, beside = TRUE, col = c( 'red', 'blue' ) );

  abline( h=.15, lty = 1, col = 'black' );
  abline( h=.20, lty = 2, col = 'red' );
  abline( h=.10, lty = 2, col = 'red' );
  title( 'Volatility' );
};

###########################################################################################
# Routine: my.pf.risk.stats
#
# Print various risk statistics for my portfolio.
###########################################################################################

my.pf.risk.stats <- function( pf = NULL ) {
  if( is.null( pf ) ) {
    pf_trade = initialize.trade();
    pf = Portfolio( pf_trade );
  };
  
  if( is.empty( OptionInfo( pf ) ) ) {
    pf = setOptionInfo( pf );
  }; 

  Implied.Vol.1m = Volatility( pf, method = 'Implied', freq = 'M', start.date = today() %m-% months(6) );

  Intraday.Vol.1d = Volatility( pf, method = 'Intraday', freq = '15m', start.date = today() - days(1) );
  Intraday.Vol.1w = Volatility( pf, method = 'Intraday', freq = '15m', start.date = today() - weeks(1) );
  Intraday.Vol.2w = Volatility( pf, method = 'Intraday', freq = '15m', start.date = today() - weeks(2) );
  Intraday.Vol.1m = Volatility( pf, method = 'Intraday', freq = '15m', start.date = today() %m-% months(1) );

  Historical.Vol.1m   = Volatility( pf, method = 'Historical', freq = 'D', start.date = today() %m-% months(1) );
  Historical.Vol.3m   = Volatility( pf, method = 'Historical', freq = 'D', start.date = today() %m-% months(3) );
  Historical.Vol.6m   = Volatility( pf, method = 'Historical', freq = 'D', start.date = today() %m-% months(6) );
  Historical.Vol.12m  = Volatility( pf, method = 'Historical', freq = 'D', start.date = today() %m-% months(12) );

  MKT.Vol.30y = Volatility( pf, freq = 'M', start.date = today() %m-% years(30), method = 'MKT' );
  FF7.Vol.30y = Volatility( pf, freq = 'M', start.date = today() %m-% years(30), method = 'FF7' );

  pf = setSimulation( pf, factor.list = 'FF7' );
  FF7.VaR = data.frame( -VaR( pf, VaR.pct = c( .01, .05, .10, .50 ) ) );
  rownames( FF7.VaR ) = paste( 'FF7.VaR', rownames( FF7.VaR ), sep = '.' );
  colnames( FF7.VaR ) = 'Risk.Stats';
  FF7.CVaR = data.frame( -CVaR( pf, VaR.pct = c( .01, .05, .10, .50 ) ) );
  rownames( FF7.CVaR ) = paste( 'FF7.CVaR', rownames( FF7.CVaR ), sep = '.' );
  colnames( FF7.CVaR ) = 'Risk.Stats';

  FF7.MER = data.frame( quantile( SimulateMarginEquityRatio( pf ), c( .99, .95, .90, .50 ) ) );
  rownames( FF7.MER ) = paste( 'FF7.MER', rownames( FF7.MER ), sep = '.' );
  colnames( FF7.MER ) = 'Risk.Stats';

  pf = setSimulation( pf, factor.list = 'MKT' );
  MKT.Beta = data.frame( quantile( SimulateBetas( pf ), c( .99, .95, .90, .50 ) ) );
  rownames( MKT.Beta ) = paste( 'MKT.Beta', rownames( MKT.Beta ), sep = '.' );
  colnames( MKT.Beta ) = 'Risk.Stats';

  curr.beta = Beta( pf );
  colnames( curr.beta ) = 'Risk.Stats';
  rownames( curr.beta ) = 'Beta';

  risk.stats = as.data.frame( t( data.frame( 
		Implied.Vol.1m, Intraday.Vol.1d, Intraday.Vol.1w, Intraday.Vol.2w, Intraday.Vol.1m, 
       		Historical.Vol.1m, Historical.Vol.3m, Historical.Vol.6m, Historical.Vol.12m,     
       		MKT.Vol.30y, FF7.Vol.30y ) ) );

  colnames( risk.stats ) = 'Risk.Stats';
  risk.stats = rbind( curr.beta, risk.stats );

  risk.stats = round( rbind( risk.stats, rbind( FF7.VaR, FF7.CVaR, FF7.MER, MKT.Beta ) ) * 1e3 ) / 10;
  return( risk.stats );
};

###########################################################################################
# Routine: save.trade.performance
###########################################################################################

save.trade.performance <- function(
  pf_trade = NULL,
  asOfDate = last.biz.date( today() ), 
  transaction.log = TRANSACTIONS_CSV, 
  trade.name = ''
) {
  if( is.null( pf_trade ) ) {
    pf_trade = initialize.trade()
  };

  time.weighted.mv <- TotalReturn( pf_trade );
  total.mv = MktVal( pf_trade, asOfDate )[ 'Total' ];

  transactions = get.pf.transactions( transaction.log = transaction.log, trade.name = trade.name );
  Symbols = sort( unique( transactions$Symbol ) );

  start.date = as.Date( min( transactions$Transaction.Date ) );

  trades = sort( unique( transactions$Trade.Name ) )
  trades = trades[ !is.na( trades ) & trades != '' & trades != 'USDCash' ];

  pnlts = mvts = ts = timeSeries(); 
  trade.stats = data.frame();
  for( i in 1:length( trades ) ) {
    trade = trades[i];
    trade_object = initialize.trade( trade );

    trade.ts = TotalReturn( trade_object )[, 'Total' ];
    ts = nanmerge( ts, timeSeries( as.numeric( trade.ts ), row.names( trade.ts ), units = trade ), rm.na = FALSE );

    tmp = PnLTS( trade_object )[, 'Total' ];
    pnlts = nanmerge( pnlts, timeSeries( as.numeric(tmp), row.names( tmp ), units = trade ), rm.na = FALSE );

    tmp = MktValTS( trade_object )[, 'Total' ];
    mvts = nanmerge( mvts, timeSeries( as.numeric(tmp), row.names( tmp ), units = trade ), rm.na = FALSE );

    key.stats = calc.key.stats( trade.ts );
    trade.stats = rbind( trade.stats, cbind( Trade.Name = trade, PnL = PnL( trade_object, asOfDate ), 
					Current.Weight = MktVal( trade_object, asOfDate )[ 'Total' ] / total.mv, key.stats ) );
  };

  performance_file = '../performance/tradeStats.xlsx';

  trade.stats[ is.na(trade.stats) ] = '';
  write.xlsx( x = t( trade.stats ), file = performance_file, sheetName = 'Trade Stats', row.names = TRUE, col.names = FALSE );

  dts = as.data.frame( ts );
  dts[ is.na(dts) ] = '';
  write.xlsx( x = dts, file = performance_file, sheetName = 'Performance', row.names = TRUE, col.names = TRUE, append = TRUE );

  dpnlts = as.data.frame( pnlts );
  dpnlts[ is.na(dpnlts) ] = '';
  write.xlsx( x = dpnlts, file = performance_file, sheetName = 'PnL', row.names = TRUE, col.names = TRUE, append = TRUE );

  write.xlsx( x = as.data.frame( MktValTS( pf_trade ) ), file = performance_file, sheetName = 'MktVal', row.names = TRUE,
					col.names = TRUE, append = TRUE );

};

###########################################################################################
# Routine: my.trade.performance
#
# Print performance for individual trades
###########################################################################################

my.trade.performance <- function( 
  pf_trade = NULL,
  asOfDate = last.biz.date( today() ), 
  Format = FALSE, 
  transaction.log = TRANSACTIONS_CSV, 
  trade.name = ''
) {

  # Create a new trade object if one is not provided
  if( is.null( pf_trade ) ) {
    pf_trade = initialize.trade()
  };

  # Get the total market value on the as-of-date
  total.mv = MktVal( pf_trade, asOfDate = asOfDate )[ 'Total' ];

  # Get the transactions fort he specific trade
  trans = get.pf.transactions( transaction.log = transaction.log, trade.name = trade.name );

  # Find the list of all trades that were not deposits/withdrawals
  trades = sort( unique( trans$Trade.Name[ !( trans$Transaction.Type %in% c('Deposit', 'Withdrawal' ) ) ] ) );

  # Disallow empty and NA trades, and add 'Total' to the list of trades to be evaluated
  trades = trades[ !is.na( trades ) & trades != '' ];
  trades = c( 'Total', trades );

  # List the PnL horizons that we want to keep
  PnL.cols = c( 'PnL.ITD', 'PnL.YTD', 'PnL.MTD', 'PnL.1m', 'PnL.1w', 'PnL.1b' );

  # Loop through all of the trades
  trade.stats = data.frame();
  for( trade in trades ) {

    if( trade == 'Total' ) {
      # If we want the Total portfolio return, just use the existing Trade object
      trade_object = pf_trade;
    } else {
      # Reset the quantities on the Trade object
      trade_object = resetTrade( pf_trade, trade.name = trade );
    };

    # Get realized risk/returns data for the trade (1m,3m,YTD for Rtn, Std, etc. )
    trade.tot.rtn.ts = TotalReturn( trade_object )[, 'Total' ];
    trade.tot.rtn.ts[ is.nan( trade.tot.rtn.ts ), ] = 0;
    print( trade );
    key.stats = calc.key.stats( trade.tot.rtn.ts );
    
    key.stats$Trade.Name = trade;
    key.stats$Current.Weight = MktVal( trade_object, asOfDate )[ 'Total' ] / total.mv;

    # Get PnL data for the trade (1m, 3m, YTD, ITD, etc. )
    pnlts = PnLTS( trade_object );
    pnl.stats = calc.key.stats( pnlts[,'Total' ] );   
    cols = colnames( pnl.stats );
    colnames( pnl.stats ) = gsub( 'Chg', 'PnL', cols );

    # Append the PnL information to the rest of the information for the trade
    new.stats = cbind( key.stats, pnl.stats[, PnL.cols ] );
    missing.cols = setdiff( colnames( trade.stats ), colnames( new.stats ) );
    if( length( missing.cols ) > 0 ) {
      fill.data = as.data.frame( matrix( NA, ncol = length( missing.cols ) ) );
      colnames( fill.data ) = missing.cols;
      new.stats = cbind( new.stats, fill.data );
      new.stats = new.stats[,colnames(trade.stats)];
    };

    # Add trade results to output
    rownames( new.stats ) = trade;
    trade.stats <- rbind( trade.stats, do.call( data.frame, new.stats ) );
  };

  # Rename the rownames of the output to be equal to the trade name
  rownames( trade.stats ) = trade.stats$Trade.Name;

  # If requested, then format the output
  if( Format == TRUE ) {
    trade.cols = colnames( trade.stats );
    SR.cols  = trade.cols[ grep( 'SR', trade.cols ) ];

    cols = c( 'Start.Date', 'End.Date', 'nYears', PnL.cols,
		'Rtn.ITD', 'Rtn.YTD', 'Rtn.MTD', 'Rtn.1m', 'Rtn.1w', 'Rtn.1b', 
		'Std.ITD', 'Std.YTD', 'Std.MTD', 'Std.1m',  SR.cols );

    trade.stats = trade.stats[, cols ];

    for( col in c( 'Start.Date', 'End.Date' ) ) {
      trade.stats[ , col ] = as.Date( as.character( trade.stats[ , col ] ), '%Y-%m-%d' );
    };

    trade.stats[ , 'nYears' ] = format( trade.stats[, 'nYears' ], digits = 2 );
    trade.stats[, SR.cols ] = format( trade.stats[, SR.cols ], digits = 2, nsmall = 2 );

    Rtn.cols = cols[ grep( 'Rtn|Current.Weight|Std', cols ) ]
    for( col in Rtn.cols ) {
       trade.stats[,col] = round( as.numeric( trade.stats[,col] ) * 1000 ) / 1000;
    };

    for( col in PnL.cols ) {
       trade.stats[,col] = round( as.numeric( trade.stats[,col] ) );
    };

    # Put the most recent trades at the top, but also put the Total row first
    tot.row = trade.stats[1,];
    non.tot.trade.stats = trade.stats[-1,]
    non.tot.trade.stats = non.tot.trade.stats[ order( non.tot.trade.stats$End.Date, decreasing = TRUE ), ];
    trade.stats = rbind( tot.row, non.tot.trade.stats );
  };

  return( trade.stats );
};

###########################################################################################
# Routine: plot.pf.stats
#
# Plot portfolio Beta, Drawdown, Return, and Margin-to-Equity Ratio
###########################################################################################

plot.pf.stats <- function( pf_trade_object = NULL ) {
  if( is.null( pf_trade_object ) ) {
    pf_trade_object = initialize.trade();
  };

  par( mfrow = c( 2, 2 ) );
  
  # Plot Time-Weighted Returns
  plot( TotalReturn( pf_trade_object ) );
  title( main = 'Time Weighted Portfolio Return', xlab = 'Date' );

  # Plot Portfolio draw-downs
  plot( TotalReturn( pf_trade_object ) / roll.fun( max, TotalReturn( pf_trade_object ) ) );
  title( main = 'Portfolio Drawdown', xlab = 'Date' );

  # Plot portfolio beta over time
  betas = BetaTS( pf_trade_object );

  plot( betas[,'Total'] / MktValTS( pf_trade_object )$Total );
  abline( h = 1, col = 'red');
  title( main = 'Portfolio Beta', xlab = 'Date' );

  # Plot Margin Equity Ratio
  plot( MarginEquityTS( pf_trade_object )[,'Total'] );
  abline( h = 0.75, col = 'red' );
  title( main = 'Margin Equity Ratio', xlab = 'Date' );
};

###########################################################################################
# Routine: plot.pf.total.return
###########################################################################################

plot.pf.total.return <- function( pf_trade_object = NULL ) {
  if( is.null( pf_trade_object ) ) {
    pf_trade_object = initialize.trade();
  };

  # Plot Time-Weighted Returns
  plot( TotalReturn( pf_trade_object ) );
  title( main = 'Time Weighted Portfolio Return', xlab = 'Date' );
};

###########################################################################################
# Routine: plot.pf.drawdown
###########################################################################################

plot.pf.drawdown <- function( pf_trade_object = NULL ) {
  if( is.null( pf_trade_object ) ) {
    pf_trade_object = initialize.trade();
  };

  # Plot Portfolio draw-downs
  plot( TotalReturn( pf_trade_object ) / roll.fun( max, TotalReturn( pf_trade_object ) ) );
  title( main = 'Portfolio Drawdown', xlab = 'Date' );
};

###########################################################################################
# Routine: plot.pf.beta
###########################################################################################

plot.pf.beta <- function( pf_trade_object = NULL ) {
  if( is.null( pf_trade_object ) ) {
    pf_trade_object = initialize.trade();
  };

  # Plot portfolio beta
  betas = BetaTS( pf_trade_object );
  plot( betas[,'Total'] / MktValTS( pf_trade_object )$Total );
  abline( h = 1, col = 'red');
  title( main = 'Portfolio Beta', xlab = 'Date' );
};

###########################################################################################
# Routine: plot.pf.margin.equity.ratio
###########################################################################################

plot.pf.margin.equity.ratio <- function( pf_trade_object = NULL ) {
  if( is.null( pf_trade_object ) ) {
    pf_trade_object = initialize.trade();
  };

  # Plot Margin Equity Ratio
  plot( MarginEquityTS( pf_trade_object )[,'Total'] );
  abline( h = 0.75, col = 'red' );
  title( main = 'Margin Equity Ratio', xlab = 'Date' );
};

###########################################################################################
# Routine: get.pf.summary
###########################################################################################

get.pf.summary <- function( 
  pf = NULL, 
  format = FALSE
) {
  if( is.null( pf ) ) {
    pf = get.portfolio();
  };

  symbols = AssetSymbols(pf);
  qty = Quantity(pf);
  mktval = MktVal(pf);
  prc = AssetPrices(pf);
  beta = AssetBetas(pf);
  colnames( beta ) = 'Beta';
  denom.ccy = get.denominated.currency( symbols );
  exp.ccy = get.denominated.currency( symbols );

  df = data.frame( Symbol = symbols, Quantity = qty, MarketValue = mktval, 
			Price = prc, Beta = beta, DenomCcy = denom.ccy, ExpCcy = exp.ccy );
  if( format ) {
    fmt.obj = list( 'char', 'Quantity', 'MarketValue', 'Price', 'Price', 'char', 'char' );
    df = format.df( df, fmt.obj );
  };
  
  return( df );
};

###########################################################################################
# Routine: get.pf.asset.summary
###########################################################################################

get.pf.asset.summary <- function( 
  pf = NULL,
  format = FALSE
) {
  if( is.null( pf ) ) {
    pf = get.portfolio();
  };
 
  df = data.frame( Symbol = AssetSymbols(pf), Quantity = Quantity(pf), 
                                MarketValue = MktVal(pf), Price = AssetPrices(pf) );

  if( format ) {
    fmt.obj = list( NULL, 'Quantity', 'MarketValue', 'Price' );
    df = format.df( df, fmt.obj );
  };

  return( df );
};



