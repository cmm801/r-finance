
source( paste( FINANCE_PATH, "/_Trade.R", sep = "" ) );

###########################################################################################
# Routine: calc.margin.equity.ratio
###########################################################################################

calc.margin.equity.ratio <- function( weights, symbols = NULL ) {
  if( is.null( dim( weights ) ) ) {
    weights = matrix( weights, nrow = 1 );
  };

  total = rowSums( weights );

  asset.margin = get.margin.amounts( symbols );

  margin = weights * matrix( asset.margin, ncol = length( asset.margin ), nrow = ifelse( is.null( dim( weights ) ), 1, nrow( weights ) ),
														 byrow = TRUE );
  margin = -pmin( as.matrix( margin ), 0 );

  margin.eq.ratio = margin / matrix( rep( total, ncol( margin ) ), ncol = ncol( margin ), byrow = FALSE )

  margin.equity.ratio = rowSums( margin.eq.ratio );

  if( class( weights ) == 'timeSeries' ) {
    margin.equity.ratio = timeSeries( margin.equity.ratio, rownames( weights ), units = 'Total' );
  };

  return( margin.equity.ratio );
};

###########################################################################################
# Routine: get.margin.amounts
###########################################################################################

get.margin.amounts <- function( symbols ) {
  
  asset.margin = rep( 0, length( symbols ) );

  for( i in 1:length( symbols ) ) {
    if( is.cash.symbol( symbols[i] ) ) {
      asset.margin[i] = 0;
    } else {
      info = get.etf.info( symbols[i] );
      lev  = info$Leverage;

      if( nrow( info ) == 0 ) {
        asset.margin[i] = 0;
        print( symbols[i] );
      } else {
        asset.margin[i] = ifelse(  is.null( lev) || is.na( lev ) || abs( lev ) - 1 < 1e-4, .50, 1 );
      };
    };
  };

  return( asset.margin );
};

###########################################################################################
# Routine: get.portfolio.exposures
###########################################################################################

get.portfolio.exposures <- function( 
  weights, 
  symbols, 
  asOfDate = last.biz.date()
) {
  underlier.symbols = c();
  underlier.weights = c();

  # Replace Option Symbols with their underliers, scaled by the 'Option Beta'
  for( k in 1:length( symbols ) ) {
    # In the first block, define the varables Symbol and Weight in the separate cases that the symbol is or is not an option.
    if( !is.option.symbol( symbols[k] ) ) {
      Symbol = symbols[k];
      Weight = weights[k];
    } else {
      option.info = parse.option.symbol( symbols[k] );
      Symbol = option.info$Symbol;
   
      leverage.ts = estimate.option.leverage( symbols[k], start.date=asOfDate - 1, asOfDate = asOfDate );
      leverage = as.numeric( tail( leverage.ts, 1 ) );
      Weight = leverage * weights[k];
    };

    if( Symbol %in% underlier.symbols ) {
      # If the Symbol is included in the array of underlier symbols, then increment its weight accordingly
      idx = which( underlier.symbols == Symbol );
      underlier.weights[ idx ] = underlier.weights[ idx ] + Weight;
    } else {
      # If Symbol is not included in the array of underliers, then add it and add its weight to the weight array
      underlier.symbols = c( underlier.symbols, Symbol );
      underlier.weights = c( underlier.weights, Weight );
    };
  };

  names( underlier.weights ) = underlier.symbols;
  return( underlier.weights );
};

###########################################################################################
# Routine: get.performance.time.series
#
# Get Equity and Option price series, and use proxies when necessary for options. 
###########################################################################################

get.performance.time.series <- function( 
  Symbols,  
  start.date = today() %m-% years(10), 
  end.date = last.biz.date(), 
  data.type = 'AdjClose', 
  BidAsk = 'Mid', 
  rm.na = FALSE,
  base.ccy = 'USD'
) {

  # Replace cash symbols by the currency pair (e.g., EURCash -> USD/EUR).  
  # The interest from cash will be included in dividends, not in prices
  cash.idx = is.cash.symbol( Symbols );
  parsed.cash = parse.cash.symbol( Symbols[ cash.idx ] );
  ccy.pairs = sprintf( '%s/%s', parsed.cash$Currency, base.ccy );
  new.symbols = Symbols;
  new.symbols[ cash.idx ] = ccy.pairs;

  # Get the time series levels
  levels = get.time.series( new.symbols, use.proxy = TRUE, start.date = start.date, end.date = end.date,
							data.type = data.type, BidAsk = BidAsk, rm.na = rm.na );

  # Multiply option prices by 100 to account for the fact that each contract is written on 100 shares
  option.symbol.inds = is.option.symbol( colnames( levels ) );
  levels[, option.symbol.inds ] = levels[, option.symbol.inds ] * 100;

  # Change the names of cash symbols back to their original
  colnames( levels ) = Symbols;

  return( levels );
};

###########################################################################################
# Routine: calc.pnl
# 
# Given a time series representing the Market Value, and another representing total inflows
# and outflows, calculate the PnL time series.
###########################################################################################

calc.pnl <- function( 
  mktval,
  flows = c()
) {
  # Remove the 'Total' column from MktVal
   mv = mktval[, colnames( mktval ) != 'Total' ];
   mv = fill.missing( mv, start.date = first.date( mktval ), end.date = last.date( mktval ), only.weekdays = TRUE );

   dates = as.Date( rownames( mv ) );

   if( identical( flows, c() ) ) {
     flows.mv = mktval * 0;
   } else {
     flow.dates = as.Date( rownames( flows ) );
     start.date = min( first.date( mv ), first.date( flows ) );
     end.date   = max( last.date( mv ),  last.date( flows ) );

     cum.flows = apply( flows, 2, cumsum );
     if( !is.timeSeries( cum.flows ) ) {
       cum.flows = timeSeries( matrix( cum.flows, ncol = ncol( flows ) ), rownames( flows ), colnames( flows ) );
     } else {
       colnames(cum.flows) = colnames(flows);
     };

     flows.mv  = fill.missing( cum.flows, only.weekdays = TRUE, start.date = start.date, end.date = end.date );
   }

   cols = colnames( flows.mv );

   flows.mv = ts.range( flows.mv, start.date = first.date(mv), end.date = last.date(mv) );
   PnL = mv[ rownames( flows.mv ), ];
   PnL[,cols] = PnL[,cols] - flows.mv[, cols ];
   Tot = timeSeries( apply( PnL, 1, sum ), rownames( flows.mv ), 'Total' );
   PnL.ts = merge( PnL, Tot );
   return( PnL.ts );
};


###########################################################################################
# Routine: calc.time.weighted.return
# 
# Given a time series representing the Market Value, and another representing total inflows
# and outflows, calculate the PnL between the start and end dates.
###########################################################################################

calc.time.weighted.return <- function(
  mktval,
  flows = c(),
  start.date = first.date( mktval ), 
  end.date   = last.date( mktval )
) {
   # If there are no Inflows/Outflows, then just normalize the market value to start at 1
   if( is.empty( flows ) ) {
     return( ind.ts( mktval ) );
   };

   # If the ending Market Value is 0, then add back the final outflow to Mkt Val
   if( last( mktval ) == 0 && last( flows ) != 0 ) {
      mktval[ nrow( mktval ) ] = -flows[ nrow( flows ) ];
      flows[ nrow( flows ) ] = 0;
   };

   dates = as.Date( rownames( mktval ) );
   total.mv <- mktval[ as.Date( start.date ) <= dates & dates <= as.Date( end.date ), ]; 

   flow.dates = as.Date( rownames( flows ) );
   rescaleTS = timeSeries( rep( 1, length( dates ) ), dates );

   # Calculate by what percentage the mktval changed with each inflow/outflow
   for( t in 1:nrow( flows ) ) {
     if( flow.dates[t] <= min( dates ) ) {
       next;
     } else {
       prev.mv <- as.numeric( total.mv[ dates[ max( which( dates < flow.dates[t] ) ) ], ] );
       rescaleTS[ which( dates == flow.dates[t] ), ] = prev.mv / ( prev.mv + as.numeric( sum( flows[ t, ] ) ) );
     };
   };
   
   # Rescale the MV so that we can compare all daily returns on equal footing
   rescale.factor = cumprod( rescaleTS );
   rescaled.mv <- ind.ts( total.mv * rescale.factor );

   # Start the time series at 1
   if( sign( total.mv[1] ) < 0 ) {
     rescaled.mv <- 2 - rescaled.mv
   };

   return( rescaled.mv ); 
};


