
setClass(
  "OptionDB",
  representation = representation(
    Symbol  = "character",
    Underlier = 'character',
    ExpirationDate = 'Date', 
    PricingDate = 'Date', 
    BidAsk = 'character',
    Currency = 'character',
    DB = 'character'
  ), 
  contains = 'OptionBS', 
  prototype = prototype( DB = MYSQL_DB_NAME, BidAsk = 'Mid', Currency = 'USD', Style = 'European' ), 
  validity = function(object) {
    # Make sure a valid option symbol is specified
    if( is.empty(object@Symbol) || !is.option.symbol(object@Symbol) ) {
      stop( 'A valid option symbol must be specified.' );
    };

    # Check that 'BidAsk' is entered correctly
    if( !is.empty(object@BidAsk) && !( object@BidAsk %in% c( 'Bid', 'Ask', 'Mid' ) ) ) {
      stop( 'If BidAsk is specified, it must be one of Bid, Ask, or Mid.' ); 
    };
  }
);

setMethod( "initialize", "OptionDB",
   function( .Object, Symbol = character(), BidAsk = character() ) {
      # Check that the option Symbol is valid
      if( is.empty(Symbol) || !is.option.symbol(Symbol) ) {
        stop( 'A valid option symbol must be specified.' );
      };

     .Object@Symbol = Symbol;
     
     # Populate fields from parsing the option symbol
     parsed.info = parse.option.symbol( Symbol );
     .Object@Underlier = parsed.info$Symbol;
     .Object@Strike = parsed.info$Strike;
     .Object@Type = parsed.info$Type;
     .Object@ExpirationDate = parsed.info$Expiration.Date;

     # Get historical option prices from the database
     db.ts.all = get.db.option.ts( .Object@Underlier, Strike = .Object@Strike, 
		Expiration.Date = .Object@ExpirationDate, Type = .Object@Type, BidAsk = .Object@BidAsk );
     # Remove NA's and weekends
     db.ts = db.ts.all[ !is.na( db.ts.all ) & wday( rownames( db.ts.all ) ) %in% 2:6, ];
     .Object@PricingDate = as.Date( rownames( db.ts ) );
     .Object@Price = as.numeric( db.ts );
     .Object@Maturity = 1/365.25 * ( as.numeric( .Object@ExpirationDate ) - as.numeric( .Object@PricingDate ) );

     # Get the dividend yield
     div.ts = get.dividend.yield.ts( .Object@Underlier, start.date = StartDate( .Object ), end.date = EndDate( .Object ) );
     div.ts = merge( div.ts, timeSeries( div.ts[1], StartDate( .Object ), colnames( div.ts ) ) );
     .Object@DivYield = as.numeric( fill.missing( div.ts, start.date = StartDate(.Object), end.date = EndDate(.Object) )[ .Object@PricingDate ] );

     # Get the risk-free rate
     rf.ts = get.interest.rate( .Object@Currency, start.date = StartDate( .Object ), end.date = EndDate( .Object ) );
     .Object@RiskFreeRate = as.numeric( fill.missing( rf.ts )[ .Object@PricingDate ] );

     # Get the underlier spot levels
     underlier.ts = get.time.series( .Object@Underlier, data.type = 'Close', start.date = StartDate( .Object ), end.date = EndDate( .Object ) );
     .Object@Spot = as.numeric( underlier.ts[ .Object@PricingDate ] );
  
     validObject(.Object);
     return( .Object );
   }
);

setMethod( 'StartDate', 'OptionDB', function( .Object ) as.Date( .Object@PricingDate[1] ) );
setMethod( 'EndDate', 'OptionDB', function( .Object ) as.Date( tail( .Object@PricingDate, 1 ) ) );






