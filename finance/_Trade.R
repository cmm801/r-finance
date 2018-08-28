
setClass(  
   "Trade", 
  representation( 
      TradeName  = "character", 
      ID         = "character", 
      TradeDates = 'Date',
      StartDate  = "Date",
      EndDate    = "Date",
      QuantityTS = "timeSeries",
      QuantityNetTS = "timeSeries",
      TradePriceTS = 'timeSeries',
      AdjustedTradePrices = 'timeSeries',
      MktValTS = "timeSeries", 
      FlowsTS  = 'timeSeries',
      IncomeTS = 'timeSeries',
      IncomePerShareTS = 'timeSeries',
      TransactionLog = 'character', 
      BaseCurrency = 'character', 
      AssetClasses = 'character', 
      Underliers = 'character', 
      DenominatedCurrency = 'character', 
      ExposureCurrency = 'character', 
      InterestRateTS = 'timeSeries',
      ClosingPriceTS = 'timeSeries'
   ), 
);

###########################################################################################
# Routine: Trade::initialize
###########################################################################################

setMethod( "initialize", "Trade",
   function( .Object, TradeName = "", StartDate = as.Date(0), EndDate = as.Date(0), TransactionLog = TRANSACTIONS_CSV ) {
     .Object@TradeName      <- TradeName;
     .Object@StartDate      <- StartDate;
     .Object@EndDate        <- EndDate;
     .Object@TransactionLog <- TransactionLog;

     # Set base currency to USD by default
     .Object@BaseCurrency = 'USD';

      return( .Object );
    }
);

setGeneric( "ID",       	def=function( .Object ) standardGeneric( "ID" ) );
setMethod(  "ID",      		"Trade", function( .Object ) .Object@ID   );

setGeneric( "BaseCurrency",    	def=function( .Object ) standardGeneric( "BaseCurrency" ) );
setMethod(  "BaseCurrency",     "Trade", function( .Object ) .Object@BaseCurrency   );

setGeneric( "QuantityTS",     	def=function( .Object ) standardGeneric( "QuantityTS" ) );
setMethod(  "QuantityTS",    	"Trade", function( .Object ) .Object@QuantityTS );

setGeneric( "QuantityNetTS",   	def=function( .Object ) standardGeneric( "QuantityNetTS" ) );
setMethod(  "QuantityNetTS",  	"Trade", function( .Object ) .Object@QuantityNetTS );

setGeneric( "IncomeTS",       def=function( .Object ) standardGeneric( "IncomeTS" ) );
setMethod(  "IncomeTS",       "Trade", function( .Object ) .Object@IncomeTS );

setGeneric( "IncomePerShareTS", def=function( .Object ) standardGeneric( "IncomePerShareTS" ) );
setMethod(  "IncomePerShareTS", "Trade", function( .Object ) .Object@IncomePerShareTS );

setGeneric( "MktValTS",     	def=function( .Object ) standardGeneric( "MktValTS" ) );
setMethod(  "MktValTS", 	"Trade", function( .Object ) .Object@MktValTS );

setGeneric( "TradeName",    	def=function( .Object ) standardGeneric( "TradeName" ) );
setMethod(  "TradeName",	"Trade", function( .Object ) .Object@TradeName );

setGeneric( "TradePriceTS",    	def=function( .Object ) standardGeneric( "TradePriceTS" ) );
setMethod(  "TradePriceTS", 	"Trade", function( .Object ) .Object@TradePriceTS );

setGeneric( "TradeDates",     	def=function( .Object ) standardGeneric( "TradeDates" ) );
setMethod(  "TradeDates",    	"Trade", function( .Object ) .Object@TradeDates );

setGeneric( "StartDate",     	def=function( .Object ) standardGeneric( "StartDate" ) );
setMethod(  "StartDate",    	"Trade", function( .Object ) .Object@StartDate );

setGeneric( "EndDate",     	def=function( .Object ) standardGeneric( "EndDate" ) );
setMethod(  "EndDate",    	"Trade", function( .Object ) .Object@EndDate );

setGeneric( "TradePriceTS",    	def=function( .Object ) standardGeneric( "TradePriceTS" ) );
setMethod(  "TradePriceTS",   	"Trade", function( .Object ) .Object@TradePriceTS );

setGeneric( "FlowsTS",   	def=function( .Object ) standardGeneric( "FlowsTS" ) );
setMethod(  "FlowsTS",  	"Trade", function( .Object ) .Object@FlowsTS );

setGeneric( "AdjustedTradePrices",  def=function( .Object ) standardGeneric( "AdjustedTradePrices" ) );
setMethod(  "AdjustedTradePrices", "Trade", function( .Object ) .Object@AdjustedTradePrices );

setGeneric( "MktVal", def=function( .Object, asOfDate = today() ) standardGeneric( "MktVal" ) );
setGeneric( "Quantity", def=function( .Object, ... ) standardGeneric( "Quantity" ) );
setGeneric( "PnLTS", def=function( .Object ) standardGeneric( "PnLTS" ) );
setGeneric( "WeightTS", def=function( .Object ) standardGeneric( "WeightTS" ) );
setGeneric( "PnL", def=function( .Object, asOfDate = last.biz.date() ) standardGeneric( "PnL" ) );
setGeneric( "Weights", def=function( .Object, asOfDate = last.biz.date() ) standardGeneric( "Weights" ) );
setGeneric( "Portfolio", def=function( .Object, asOfDate = .Object@EndDate ) standardGeneric( "Portfolio" ) );
setGeneric( "MarginEquityTS", def=function( .Object, start.date = .Object@StartDate, end.date = .Object@EndDate ) 
											standardGeneric( "MarginEquityTS" ) );
setGeneric( "BetaTS", def=function( .Object, start.date = .Object@StartDate, end.date = .Object@EndDate ) 
											standardGeneric( "BetaTS" ) );
setGeneric( "TotalReturn",def=function( .Object, start.date = NA, end.date = NA ) standardGeneric( "TotalReturn" ) );
setGeneric( "setMktValTS", def=function( .Object ) standardGeneric( "setMktValTS" ) );
setGeneric( "setTradePriceTS",  def=function( .Object ) standardGeneric( "setTradePriceTS" ) );
setGeneric( "setQuantityTS",  def=function( .Object ) standardGeneric( "setQuantityTS" ) );
setGeneric( "setIncomeTS",  def=function( .Object ) standardGeneric( "setIncomeTS" ) );
setGeneric( "setInterestRateTS",  def=function( .Object ) standardGeneric( "setInterestRateTS" ) );
setGeneric( "setFlowsTS",  def=function( .Object ) standardGeneric( "setFlowsTS" ) );
setGeneric( "PnLSummary", def=function( .Object, start.date = .Object@StartDate, end.date = .Object@EndDate ) 
						standardGeneric( "PnLSummary" ) );

setGeneric( "resetQuantityTS",  def=function( .Object, quantity.ts ) standardGeneric( "resetQuantityTS" ) );
setGeneric( "resetTrade",  def=function( .Object, trade.name ) standardGeneric( "resetTrade" ) );
setGeneric( "addIncomeToQuantityNetTS",  def=function( .Object ) standardGeneric( "addIncomeToQuantityNetTS" ) );
setGeneric( "setAdjustedTradePrices",  def=function( .Object ) standardGeneric( "setAdjustedTradePrices" ) );

###########################################################################################
# Routine: Trade::setTradePriceTS
###########################################################################################

setMethod(  "setTradePriceTS", "Trade",
  function( .Object ) {
    trade.prices = get.pf.trade.prices( transaction.log = .Object@TransactionLog, trade.name = .Object@TradeName );
    .Object@TradePriceTS = trade.prices;
    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::setInterestRateTS
###########################################################################################

setMethod(  "setInterestRateTS", "Trade",
  function( .Object  ) {
    cash.tickers = .Object@ID[ .Object@AssetClasses == 'Cash' ];

    currencies = parse.cash.symbol( cash.tickers )$Currency;
    interest.rates = get.interest.rates( currencies, start.date = .Object@StartDate, end.date = .Object@EndDate );

    # Replace column names with cash names
    colnames( interest.rates ) = paste( currencies, 'Cash', sep = '' );
 
    .Object@InterestRateTS = interest.rates;
    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::setIncomeTS
###########################################################################################

setMethod(  "setIncomeTS", "Trade",
  function( .Object ) {

    # Calculate interest earned
    cash.symbols = .Object@ID[ .Object@AssetClasses == 'Cash' ];
    SD = last.biz.date( .Object@StartDate - 1 );
    cash.ts = get.time.series.cash( cash.symbols, start.date = SD, end.date = .Object@EndDate );
    interest = ri( cash.ts[ as.Date( c( SD, .Object@TradeDates ) ), ] );
 
    # Get dividend data if it hasn't been provided
    dividends = get.dividends.ts( .Object@ID, start.date = .Object@StartDate, end.date = .Object@EndDate );

    if( !is.empty( dividends ) ) {
      # Fill in missing dates for dividends
      missing.div.dates = as.Date( setdiff( as.Date( .Object@TradeDates ), as.Date( rownames( dividends ) ) ) );
      div.ts = insert.dates.ts( dividends, missing.div.dates, 0 );

      # Append interest earned to Dividends
      div.plus.interest = merge( div.ts, interest );
    } else {
      div.plus.interest = interest;
    };

    # Set income
    .Object@IncomePerShareTS = div.plus.interest;
    .Object@IncomeTS = div.plus.interest * .Object@QuantityNetTS[,colnames(div.plus.interest)];
    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::setFlowsTS
###########################################################################################

setMethod(  "setFlowsTS", "Trade",
  function( .Object ) {

    # Get inflow/outflow info, if it hasn't been provided
    flows.ts = get.pf.flows( trade.name = .Object@TradeName, transaction.log = .Object@TransactionLog,
                  start.date = .Object@StartDate, asOfDate = .Object@EndDate, base.ccy = .Object@BaseCurrency );

    # Fill in missing dates for flows
    missing.flow.dates = as.Date( setdiff( as.Date( .Object@TradeDates ), as.Date( rownames( flows.ts ) ) ) );
    zero.flow.matrix = matrix( 0, ncol = ncol(flows.ts), nrow = length(missing.flow.dates) );
    flows.fill.ts = timeSeries( zero.flow.matrix, missing.flow.dates, units = colnames(flows.ts) );

    # Set Flows and return object
    if( !is.empty( flows.fill.ts ) ) {
      .Object@FlowsTS = merge( flows.ts, flows.fill.ts );
    } else {
      .Object@FlowsTS = flows.ts;
    };

    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::setQuantityTS
###########################################################################################

setMethod(  "setQuantityTS", "Trade", 
  function( .Object ) {

    # Get the portfolio quantities without accounting for dividends / interest
    raw.quantities = get.pf.quantities( trade.name = .Object@TradeName, transaction.log = .Object@TransactionLog );
    raw.quantities = fill.missing( raw.quantities, only.weekdays = TRUE );

    # Set ID and date parameters
    .Object@ID = unique( colnames( raw.quantities ) );

   # If the StartDate hasn't been set, then use the first trade date 
   if( .Object@StartDate == as.Date(0) ) {
     .Object@StartDate = as.Date( min( as.Date(rownames(raw.quantities) ) ) );
   };

   # If the EndDate hasn't been set, use the last business date
   if( .Object@EndDate == as.Date(0) ) {
     .Object@EndDate = last.biz.date()
   };

    # Get the dividend info so we know on which dates dividends were paid out
    dividends = get.dividends.ts( .Object@ID, start.date = .Object@StartDate, end.date = .Object@EndDate );
    if( !is.empty( dividends ) ) {
      div.dates = as.Date( rownames( dividends ) );
    } else { 
      div.dates = NULL;
    };

     # Set the trade dates
     qty.dates = as.Date( sort( unique( as.Date( rownames( raw.quantities ) ) ) ) );
    .Object@TradeDates = sort( as.Date( unique( c( div.dates, qty.dates ) ) ) );

   # Set Asset classes
   .Object@AssetClasses = get.asset.class( .Object@ID );

   # Set underliers 
   .Object@Underliers = get.underliers( .Object@ID );

   # Set denominated currencies
   .Object@DenominatedCurrency = get.denominated.currency( .Object@ID );

   # Set exposure currency
   .Object@ExposureCurrency = get.exposure.currency( .Object@ID );

   # Set interest rate time series
   .Object = setInterestRateTS( .Object );

   # Set the trade prices for the portfolio
   .Object = setTradePriceTS( .Object );

   # Save QuantityNetTS (without dividends)
   if( !is.empty( div.dates ) ) {
     .Object@QuantityNetTS = fill.missing.pf.quantities( raw.quantities, div.dates );
   } else {
     .Object@QuantityNetTS = raw.quantities;
   };

   # Set dividends
   .Object = setIncomeTS( .Object );
   
   # Set Flows
   .Object = setFlowsTS( .Object );

   # Set the QuantityTS to be the net quantities plus all interest and dividends
   .Object@QuantityTS = addIncomeToQuantityNetTS( .Object );

   return( .Object )
 }
)


###########################################################################################
# Routine: Trade::addIncomeToQuantityNetTS
###########################################################################################

setMethod(  "addIncomeToQuantityNetTS", "Trade",
  function( .Object ) {

    # Add dividends and cash interest back into the appropriate cash column
    uniq.denom.ccy = unique( .Object@DenominatedCurrency );
    qty = .Object@QuantityNetTS;
    for( i in 1:length(uniq.denom.ccy) ) {
      denom.ccy = uniq.denom.ccy[i];
      denom.cash = paste( denom.ccy, 'Cash', sep = '' );
      denom.id = .Object@ID[ .Object@DenominatedCurrency == denom.ccy ];

      div.ts = .Object@IncomeTS[, colnames( .Object@IncomeTS ) %in% denom.id ];
      if( !is.empty(div.ts) ) {
        qty[,denom.cash] = qty[,denom.cash] + as.numeric( cumsum( rowSums( div.ts ) ) );
      };
    };

    return( qty );
  }
);


###########################################################################################
# Routine: Trade::setAdjustedTradePrices
###########################################################################################

setMethod(  "setAdjustedTradePrices", "Trade",
  function( .Object ) {

    # Start with the actual closing prices
    adj.prices = .Object@ClosingPriceTS;

    # Get the trade prices
    trade.prc = .Object@TradePriceTS;

    for( dt in rownames( trade.prc ) ) {
      date = as.Date( dt );
      for( col in colnames( trade.prc ) ) {
        if( is.cash.symbol( col ) ) {
          next;
        };
        if( !is.na( as.numeric( trade.prc[ date, col ] ) ) ) {
          i = which( date == as.Date( rownames( adj.prices ) ) );
          adj.prices[ i, col ] = as.numeric( trade.prc[ date, col ] );
        };
      };
    };

    .Object@AdjustedTradePrices = adj.prices;
    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::resetTrade
# 
# Set a new trade name, and reload the weights and recalculate the market value
###########################################################################################

setMethod(  "resetTrade", "Trade",
  function( .Object, trade.name ) {
    .Object@TradeName = trade.name;
    quantities = get.pf.quantities( trade.name = .Object@TradeName, transaction.log = .Object@TransactionLog );
    .Object = resetQuantityTS( .Object, quantities );
    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::resetQuantityTS
###########################################################################################

setMethod(  "resetQuantityTS", "Trade",
  function( .Object, quantity.ts ) {
     # Make sure the quantity time series has the correct dates, consistent with the start/end date
     qty.ts = fill.missing.pf.quantities( quantity.ts, .Object@TradeDates );
     qty.ts = ts.range( qty.ts, start.date = .Object@StartDate, end.date = .Object@EndDate );
     
     # Add 0's columns for missing symbols
     missing.symbols = setdiff( .Object@ID, colnames( qty.ts ) );
     if( length( missing.symbols ) > 0 ) {
       zero.df = matrix( 0, ncol = length( missing.symbols ), nrow = length( .Object@TradeDates ) );
       zero.ts = timeSeries( zero.df, rownames( qty.ts ), units = missing.symbols ); 
       quantities = merge( qty.ts, zero.ts );
     } else {
       quantities = qty.ts;
     };

     # Put the columns into the same order as the ID's
     quantities = quantities[,.Object@ID];

     # Set the net quantities (without dividends/interest)
    .Object@QuantityNetTS = quantities;

     # Calculate the total income for the new allocation
     income.per.share = .Object@IncomePerShareTS;
    .Object@IncomeTS = income.per.share * .Object@QuantityNetTS[,colnames(income.per.share)];
    print( 'WARNING: Multiply the daily income/dividends by the quantity on the prior day.' );
    
     # Find the new quantities, including income
    .Object@QuantityTS = addIncomeToQuantityNetTS( .Object );

     # Reset the flows
     .Object = setFlowsTS( .Object );
   
     # Calculate the new market values for the allocation 
    .Object@MktValTS = ts.range( construct.mktval( .Object@QuantityTS, AdjustedTradePrices( .Object ) ),
                                        start.date = .Object@StartDate, end.date = .Object@EndDate );
    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::setMktValTS
###########################################################################################

setMethod(  "setMktValTS", "Trade", 
  function( .Object ) { 

    # Set the asset class quantities
    .Object = setQuantityTS( .Object );

    # Get the prices for the relevant asset classes
    prices = get.performance.time.series( .Object@ID, start.date = .Object@StartDate, end.date = .Object@EndDate, data.type = 'Close');
   
    # Keep prices constant when they are missing
    prices = fill.missing( prices, only.weekdays = TRUE, end.date = .Object@EndDate );
    prices = prices[ rownames( prices ) <= .Object@EndDate, ];

    # Set closing prices
    .Object@ClosingPriceTS = prices;

    # Get actual trade prices
    prc = .Object@TradePriceTS;
  
    # Set adjusted trade prices
    .Object = setAdjustedTradePrices( .Object );
 
    # Calculate the market value, using the trade price instead of closing price when available
    .Object@MktValTS = ts.range( construct.mktval( .Object@QuantityTS, AdjustedTradePrices( .Object ) ), 
					start.date = .Object@StartDate, end.date = .Object@EndDate );
    return( .Object );
  }
);

###########################################################################################
# Routine: Trade::PnL
###########################################################################################

setMethod(  "PnL", "Trade",
  function( .Object, asOfDate = last.biz.date() ) {
    pnl = find.last.date.value( PnLTS( .Object )[,'Total' ], asOfDate, method = 'constant' );
    if( is.nan( pnl ) ) {
      pnl = tail( PnLTS( .Object )$Total, 1 );
    };
    return( pnl );
  }
);

###########################################################################################
# Routine: Trade::MktVal
###########################################################################################

setMethod(  "MktVal", "Trade",
  function( .Object, asOfDate = last.biz.date() ) {
    mktval = find.last.date.value( MktValTS( .Object ), asOfDate, method = 'constant' );
    return( mktval );
  }
);

###########################################################################################
# Routine: Trade::Quantity
###########################################################################################

setMethod(  "Quantity", "Trade",
  function( .Object, asOfDate = last.biz.date() ) {
    qty = find.last.date.value( QuantityTS( .Object ), asOfDate, method = 'constant' );
    return( qty );
  }
);

###########################################################################################
# Routine: Trade::WeightTS
###########################################################################################

setMethod(  "WeightTS", "Trade",
  function( .Object ) {
    weight.ts = .Object@MktValTS[, colnames( .Object@MktValTS ) != 'Total' ] / .Object@MktValTS$Total;
    return( weight.ts );
  }
);

###########################################################################################
# Routine: Trade::Weights
###########################################################################################

setMethod(  "Weights", "Trade",
  function( .Object, asOfDate = last.biz.date() ) {
    qty = find.last.date.value( WeightTS( .Object ), asOfDate, method = 'constant' );
    return( qty );
  }
);

###########################################################################################
# Routine: Trade::PnLTS
###########################################################################################

setMethod(  "PnLTS", "Trade",
  function( .Object ) {
    pnl = calc.pnl( mktval = MktValTS( .Object ), flows = FlowsTS( .Object ) );
    return( pnl );
  }
);

###########################################################################################
# Routine: Trade::Portfolio
###########################################################################################

setMethod(  "Portfolio", "Trade",
  function( .Object, asOfDate = .Object@EndDate ) {

    # Get Trade-specific quantities
    trade.qty = as.matrix( get.trade.quantities( asOfDate ) );
    trade.symbols = rownames( trade.qty );
    asset.symbols = colnames( trade.qty );

    # Get Mapping between individual trades and assets
    mktval = matrix( MktVal( .Object, asOfDate )[match(asset.symbols,names(MktVal(.Object)) )], ncol = 1 );
    qty = matrix( Quantity( .Object, asOfDate )[match(asset.symbols,names(Quantity(.Object)) )], ncol = 1 );

    # Create a new Portfolio object
    pf = new( 'Portfolio', Symbols = asset.symbols, MktVal = mktval, Quantity = qty, 	
				TradeSymbols = trade.symbols, TradeQuantity = trade.qty );

    # Set the AsOfDate on the Portfolio object
    pf@AsOfDate = asOfDate;
 
    if( any( is.na( AssetPrices( pf ) ) ) ) {
      stop( 'Trade::Portfolio - Missing asset prices' );
    };

    return( pf );
  }
);

###########################################################################################
# Routine: Trade::TotalReturn
###########################################################################################

setMethod(  "TotalReturn", "Trade",
  function( .Object, start.date = .Object@StartDate, end.date = .Object@EndDate ) {
    mktval = MktValTS( .Object )[, 'Total' ];
    flows  = FlowsTS( .Object );

    # Only calculate performance during the trade's performance
    idx = ( as.numeric( rowSums( abs( .Object@QuantityNetTS ) ) ) != 0 );
    if( !any(idx) ) {
      adj.return = timeSeries( 1, .Object@StartDate, units = 'Total' );
    } else {
      adj.return = calc.time.weighted.return( mktval[idx,], flows = flows[idx,], 
					start.date = start.date, end.date = end.date );   
    };

    return( adj.return );
  }
);

###########################################################################################
# Routine: Trade::BetaTS
#
# Get a time series of the Market beta for the trades.
# This calculation assumes the equity betas stay constant over time, but that the Option
#     betas are changing.
###########################################################################################

setMethod(  "BetaTS", "Trade",
  function( .Object, start.date = .Object@StartDate, end.date = .Object@EndDate ) {

    # Get Portfolio Market Value
    mv = MktValTS( .Object );

    # Exclude the Total column
    mktval = mv[, colnames( mv ) != 'Total' ];
    symbols = colnames( mktval );

    # Split symbols into Equity and Option 
    symbol.list = get.asset.class.list( symbols );

    # Get option beta time series
    option.beta.ts = estimate.option.beta( symbol.list$Option, start.date = start.date, factor.list = 'MKT' );
    option.beta.ts[ is.na( option.beta.ts ) ] = 0;

    # Calculate FX Forward betas
    deriv.beta.ts = option.beta.ts; 
    print( 'WARNING: FX Forward betas are currently set to zero' );
    if( !is.empty( symbol.list$CurrencyForward ) ) {
      for( i in 1:length( symbol.list$CurrencyForward ) ) {
        deriv.beta.ts = cbind( deriv.beta.ts, timeSeries( c(0,0), c(start.date, end.date), units = symbol.list$CurrencyForward[i] ) );
      };
    };

    # For Equity, Cash, and FX symbols:
    # Assume the beta is constant throughout time, and construct a time series with the same dates as the Option betas
    non.deriv.betas = get.beta( c( symbol.list$Equity, symbol.list$Cash, symbol.list$CurrencyPair ), factor.list = 'MKT' );
    neqs = length( non.deriv.betas[,1] );
    non.deriv.beta.mat = matrix( rep( non.deriv.betas[,1], nrow( deriv.beta.ts ) ), ncol = neqs, nrow = nrow( deriv.beta.ts ), byrow = T );
    non.deriv.beta.ts = timeSeries( non.deriv.beta.mat, rownames( deriv.beta.ts ), units = rownames( non.deriv.betas ) ); 

    # Combine equity and option time series 
    beta.ts = cbind( non.deriv.beta.ts, deriv.beta.ts ); 
    filled.beta.ts = fill.missing( beta.ts );
    full.beta.ts = filled.beta.ts[ rownames( mktval ), ];

    # Calculate the dollar beta for Equity and Option Symbols, and then merge
    dollar.beta.ts = mktval * as.matrix( full.beta.ts[,colnames( mktval ) ] ); 
    total = rowSums( dollar.beta.ts );
    beta = cbind( total, dollar.beta.ts );
    colnames( beta )[1] = 'Total';

    return( beta );
  }
);

###########################################################################################
# Routine: Trade::MarginEquityTS
###########################################################################################

setMethod(  "MarginEquityTS", "Trade",
  function( .Object, start.date = .Object@StartDate, end.date = .Object@EndDate ) {
    mktval = MktValTS( .Object );
    mktval = mktval[, colnames( mktval ) != 'Total' ];
    margin = calc.margin.equity.ratio( mktval, colnames( mktval ) );
    return( margin );
  }
);

###########################################################################################
# Routine: Trade::PnLSummary
###########################################################################################

setMethod(  "PnLSummary", "Trade",
  function( .Object, start.date = .Object@StartDate, end.date = .Object@EndDate ) {

    # Get Total Market Value
    mv.ts  = MktValTS( .Object )[,'Total'];
    mv.tot = as.numeric( tail( mv.ts, 1 ) );

    # Get Margin/Equity ratio
    margin.to.equity = as.numeric( tail( MarginEquityTS( .Object )$Total, 1 ) );

    # Get Period end dates
    year.end  = last.biz.date( max( find.all.period.ends( end.date %m-% years(1), end.date, period = 'year' ) ) );
    month.end = last.biz.date( end.date %m-% months(1) );

    # Get PnL over several windows
    pnl.ts = PnLTS(.Object)[,'Total'];
    pnl.tot = as.numeric( tail( pnl.ts, 1 ) );

    # Get flows over different windows
    flows = FlowsTS( .Object );
    flows.ytd = sum( as.numeric( flows[ as.Date( rownames( flows ) ) >= year.end ] ) );
    flows.1m  = sum( as.numeric( flows[ as.Date( rownames( flows ) ) >= month.end ] ) );
    dpnl = diff( pnl.ts )[-1,];

    # Get YTD and MTD PnL
    mv.year.end = find.last.date.value( mv.ts, year.end );
    mv.1m.ago = find.last.date.value( mv.ts, month.end );
    pnl.ytd = mv.tot - mv.year.end - flows.ytd;
    pnl.1m  = mv.tot - mv.1m.ago - flows.1m ;

    # Format output
    MktVal  = formatC( mv.tot,  format = 'd', digits = 0, big.mark = ',' );
    PNL.ITD = formatC( pnl.tot, format = 'd', digits = 0, big.mark = ',' );
    PNL.YTD = formatC( pnl.ytd, format = 'd', digits = 0, big.mark = ',' );
    PNL.1m  = formatC( pnl.1m , format = 'd', digits = 0, big.mark = ',' );

    Margin.To.Equity.Ratio = formatC( margin.to.equity * 100,  format = 'f', digits = 1, big.mark = ',' );

    Mean.1d.Win  = formatC( mean( dpnl[ dpnl > 0 ] ), format = 'd', digits = 0, big.mark = ',' );
    Mean.1d.Loss = formatC( mean( dpnl[ dpnl < 0 ] ), format = 'd', digits = 0, big.mark = ',' );

    Median.1d.Win  = formatC( median( dpnl[ dpnl > 0 ] ), format = 'd', digits = 0, big.mark = ',' );
    Median.1d.Loss = formatC( median( dpnl[ dpnl < 0 ] ), format = 'd', digits = 0, big.mark = ',' );

    Std.1d.Win  = formatC( sd( dpnl[ dpnl > 0 ] ), format = 'd', digits = 0, big.mark = ',' );
    Std.1d.Loss = formatC( sd( dpnl[ dpnl < 0 ] ), format = 'd', digits = 0, big.mark = ',' );

    Max.1d.Win  = formatC( max( dpnl ), format = 'd', digits = 0, big.mark = ',' );
    Max.1d.Loss = formatC( min( dpnl ), format = 'd', digits = 0, big.mark = ',' );

    # Combine output into data.frame  
    stats = data.frame(
                        MktVal, PNL.ITD, PNL.YTD, PNL.1m, Margin.To.Equity.Ratio, 
                        Mean.1d.Win, Mean.1d.Loss, Median.1d.Win, Median.1d.Loss, Std.1d.Win, Std.1d.Loss, Max.1d.Win, Max.1d.Loss );

    stats = as.data.frame( t( stats ) );
    colnames( stats ) = '';
    return( stats );
  }
);


