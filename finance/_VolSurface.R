
setClass(
  "VolSurface",
  representation = representation(
    ExpirationDate = 'Date', 
    Underlier = 'character',
    OptionSymbols  = "character",
    Spot = 'numeric', 
    PricingDate = 'Date', 
    BidAsk = 'character',
    Currency = 'character',
    DB = 'character'
  ), 
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






