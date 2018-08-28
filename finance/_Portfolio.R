
setClass(  
   "Portfolio", 
  representation( 
      Symbols  = "character", 
      Quantity = 'matrix', 
      MktVal   = 'matrix', 
      OptionInfo = 'data.frame', 
      FactorList = 'character', 
      Simulation = 'Simulation',
      TradeSymbols = 'character',
      TradeQuantity = 'matrix', 
      BaseCurrency = 'character', 
      ExposureCurrency = 'character',
      AssetClass = 'character', 
      AsOfDate = 'Date'
   ), 
);

setMethod( "initialize", "Portfolio",
   function( .Object, Symbols = NULL, MktVal = matrix(), Quantity = matrix(),
	TradeSymbols = Symbols, TradeQuantity = matrix() ) {

     # Order the Symbols by asset class
     ac.list = get.asset.class.list( Symbols );
     ordered.symbols = c();
     for( j in 1:length(ac.list) ) {
       ordered.symbols = c( ordered.symbols, ac.list[[j]] );
     };

     # Find the index to get the desired asset class ordering
     sort.idx = match( ordered.symbols, Symbols );

     # Set the properties equal to inputs that are provided
     .Object@Symbols <- Symbols[ sort.idx ];
     .Object@MktVal  <- as.matrix( MktVal[ sort.idx ] );

     # By default, assume USD base currency
     .Object@BaseCurrency = 'USD';

     if( is.empty( Quantity ) ) {
       # Set all Quantities to 1 if the info is not provided
       .Object@Quantity <- rep( 1, length( MktVal ) );
     } else {
       .Object@Quantity <- as.matrix( Quantity[ sort.idx ] );
     };

     .Object@TradeSymbols <- TradeSymbols;
     if( !is.empty( TradeQuantity ) ) {
       .Object@TradeQuantity <- TradeQuantity;
     };

     # Set exposure currency info
     .Object@ExposureCurrency = get.exposure.currency( Symbols( .Object ) );

     # Set other standard properties
     .Object@AssetClass = get.asset.class( Symbols( .Object ) );
     .Object@FactorList = 'MKT';
     .Object@Simulation = new( 'Simulation', symbols = Symbols( .Object ) );

     return( .Object );
   }
);

setGeneric( "AsOfDate", def=function( .Object ) standardGeneric( "AsOfDate" ) );
setMethod(  "AsOfDate", "Portfolio", function( .Object ) .Object@AsOfDate );

setGeneric( "Symbols", def=function( .Object ) standardGeneric( "Symbols" ) );
setMethod(  "Symbols", "Portfolio", function( .Object ) .Object@Symbols );

setGeneric( "BaseCurrency", def=function( .Object ) standardGeneric( "BaseCurrency" ) );
setMethod(  "BaseCurrency", "Portfolio", function( .Object ) .Object@BaseCurrency );

setGeneric( "ExposureCurrency", def=function( .Object ) standardGeneric( "ExposureCurrency" ) );
setMethod(  "ExposureCurrency", "Portfolio", function( .Object ) .Object@ExposureCurrency );

setGeneric( "AssetSymbols", def=function( .Object ) standardGeneric( "AssetSymbols" ) );
setMethod(  "AssetSymbols", "Portfolio", function( .Object ) .Object@Symbols );

setGeneric( "TradeSymbols", def=function( .Object ) standardGeneric( "TradeSymbols" ) );
setMethod(  "TradeSymbols", "Portfolio", function( .Object ) .Object@TradeSymbols );

setGeneric( "AssetClass", def=function( .Object ) standardGeneric( "AssetClass" ) );
setMethod(  "AssetClass", "Portfolio", function( .Object ) .Object@AssetClass );

setGeneric( "TradeQuantity", def=function( .Object ) standardGeneric( "TradeQuantity" ) );
setMethod(  "TradeQuantity", "Portfolio", 
  function( .Object ) {
    trade.qty = .Object@TradeQuantity;
    cash.cols = AssetSymbols(.Object)[ is.cash.symbol(AssetSymbols(.Object) ) ];
    for( i in 1:length(cash.cols) ) { 
      trade.qty[ cash.cols[i],cash.cols[i]] = Quantity(.Object)[ AssetSymbols(.Object) == cash.cols[i] ];
    };

    return(trade.qty);
  }
);

setGeneric( "MVWeightedBeta", def=function( .Object ) standardGeneric( "MVWeightedBeta" ) );
setMethod(  "MVWeightedBeta", "Portfolio",
  function( .Object ) {
    betas = AssetBetas( .Object );
    MV = MktVal( .Object );
    mv.wtd.betas = betas * MV;
    colnames( mv.wtd.betas ) = 'MV.Wtd.Betas';
    return(mv.wtd.betas);
  }
);

setGeneric( "NotionalWeightedBeta", def=function( .Object ) standardGeneric( "NotionalWeightedBeta" ) );
setMethod(  "NotionalWeightedBeta", "Portfolio",
  function( .Object ) {
    betas = AssetBetas( .Object );
    NMV = NotionalMktVal( .Object );
    notional.wtd.betas = betas * NMV;
    colnames( notional.wtd.betas ) = 'Notional.Wtd.Betas';
    return(notional.wtd.betas);
  }
);

setGeneric( "Summary", def=function( .Object, cols = NULL ) standardGeneric( "Summary" ) );
setMethod(  "Summary", "Portfolio", 
  function( .Object, cols = NULL ) {

     # Get asset betas, and change the column name
     betas = AssetBetas( .Object );
     colnames( betas ) = 'Beta';

     # Risk Attribution
     risk.attr = AssetRiskAttribution( .Object );
     colnames( risk.attr ) = 'RiskAttribution';

     summary.df = data.frame( 
				Symbol = AssetSymbols( .Object ), 
				Underlier = UnderlierSymbols( .Object ), 
				ExposureCurrency = ExposureCurrency( .Object ), 
				AssetClass = AssetClass( .Object ), 
 				Weight = AssetWeights( .Object ),
				UnderlierWeights( .Object ), 
 				MarketValue = MktVal( .Object ), 
				NotionalMktVal = NotionalMktVal( .Object ), 
				Quantity = Quantity( .Object ), 
				Leverage = Leverage( .Object ), 
				Beta = betas, 
  				MVWtdBeta = MVWeightedBeta( .Object ), 
  				NotionalWtdBeta = NotionalWeightedBeta( .Object ), 
 				RiskAttribution = risk.attr
			    );

     # Get the asset classification for assets in the portfolio
     ac = get.asset.classification( AssetSymbols( .Object ) );
     schema.cols = colnames(ac)[ regexpr( 'Schema', colnames( ac ) ) != -1 ];
     for( schema.col in schema.cols ) {
       summary.df = cbind( summary.df, ac[ schema.col ] );
     };

     if( !is.null( cols ) ) {
       return( summary.df[,cols] );
     } else {
       return( summary.df );
     };
  }
);

setGeneric( "Leverage", def=function( .Object ) standardGeneric( "Leverage" ) );
setMethod(  "Leverage", "Portfolio",
  function( .Object ) {
     opt.info = OptionInfo( .Object );
     lvg = opt.info$Leverage;
     return(lvg);
  }
);

setGeneric( "AssetPrices", def=function( .Object ) standardGeneric( "AssetPrices" ) );
setMethod(  "AssetPrices", "Portfolio", 
  function( .Object ) {
     prc = MktVal(.Object) / ( Quantity(.Object) + 1e-10 );
     colnames(prc) = 'AssetPrices';  
     return(prc);
  } 
);

setGeneric( "TradeMktVal", def=function( .Object ) standardGeneric( "TradeMktVal" ) );
setMethod(  "TradeMktVal", "Portfolio",
  function( .Object ) {
    trade.mv = TradeQuantity(.Object) %*% AssetPrices(.Object);
    return( trade.mv );
  }
);

setGeneric( "TradeWeights", def=function( .Object ) standardGeneric( "TradeWeights" ) );
setMethod(  "TradeWeights", "Portfolio", 
  function( .Object ) {
    trade.weights = TradeMktVal( .Object ) / sum( MktVal( .Object ) );
    return( trade.weights );
  }
);

setGeneric( "OptionInfo", def=function( .Object ) standardGeneric( "OptionInfo" ) );
setMethod(  "OptionInfo", "Portfolio", function( .Object ) .Object@OptionInfo );

setGeneric( "Symbols", def=function( .Object ) standardGeneric( "Symbols" ) );
setMethod(  "Symbols", "Portfolio", function( .Object ) .Object@Symbols );

setGeneric( 'getMktVal', def = function( .Object, symbol ) standardGeneric( 'getMktVal' ) );
setMethod( 'getMktVal', 'Portfolio',
  function( .Object, symbol ) {
    mktval = .Object@MktVal;
    mv = mktval[ symbol == Symbols(.Object)];
    return(mv);
  }
);

setGeneric( 'getQuantity', def = function( .Object, symbol ) standardGeneric( 'getQuantity' ) );
setMethod( 'getQuantity', 'Portfolio',
  function( .Object, symbol ) {
    quantity = .Object@Quantity;
    qty = quantity[ symbol == Symbols(.Object)];
    return(qty);
  }
);

setGeneric( 'getOptionInfoSingleAsset', def = function( .Object, symbol ) standardGeneric( 'getOptionInfoSingleAsset' ) );
setMethod( 'getOptionInfoSingleAsset', 'Portfolio',
  function( .Object, symbol ) {
    option.info = data.frame( Symbol = symbol, Underlier = symbol, Leverage = 1 );
    rownames( option.info ) = symbol;

    if( is.option.symbol( symbol ) ) {
      underlier = parse.option.symbols( symbol )$Symbol;
      leverage = as.numeric( tail( estimate.option.leverage( symbol ), 1 ) );

      option.info$Underlier = underlier;
      option.info$Leverage  = leverage;
    } else if( is.fx.forward.symbol( symbol ) ) {
      info = parse.fx.forward.symbols( symbol );
      option.info$Underlier = info$Currency.Cross;

      # If the asset is not denominated in the base currency, then convert it
      if( .Object@BaseCurrency == info$Long.Currency ) {
        base.ccy.qty = getQuantity(.Object, symbol );
      } else {
        base.ccy.qty = convert.currency( getQuantity(.Object, symbol ), 
			from.ccy = info$Long.Currency, to.ccy = .Object@BaseCurrency );
      };

      option.info$Leverage  = base.ccy.qty / getMktVal(.Object, symbol );
    };

    return( option.info );
  }
);

setGeneric( 'setOptionInfo', def = function( .Object ) standardGeneric( 'setOptionInfo' ) );
setMethod( 'setOptionInfo', 'Portfolio', 
  function( .Object ) {
    symbols = AssetSymbols( .Object );

    for( i in 1:length( symbols ) ) {
      if( i == 1 ) {
        option.info = getOptionInfoSingleAsset( .Object, symbols[i] );
      } else {
        option.info[i,] = getOptionInfoSingleAsset( .Object, symbols[i] );
      };
    };

    .Object@OptionInfo = option.info;
    return( .Object );
  }
);

setGeneric( "AssetToUnderlierMap", def=function( .Object ) standardGeneric( "AssetToUnderlierMap" ) );
setMethod(  "AssetToUnderlierMap", "Portfolio", 
  function( .Object ) {
    option.info = OptionInfo( .Object );
    symbols = AssetSymbols( .Object );

    underliers = unique( option.info$Underlier );
    asset.underlier.map = matrix( 0, nrow = length(symbols), ncol = length(underliers) );
    colnames( asset.underlier.map ) = underliers;
    rownames( asset.underlier.map ) = symbols;

    for( i in 1:length( symbols ) ) {
      asset.underlier.map[i,option.info[i,'Underlier']] = option.info[i,'Leverage'];
    };

    return( asset.underlier.map );
  }
);

setMethod(  "MktVal", "Portfolio", 
  function( .Object, asOfDate = today() ) {
    mktval = matrix( .Object@MktVal, ncol = 1 );
    rownames(mktval) = AssetSymbols( .Object );
    colnames(mktval) = 'MarketValue';
    return(mktval);
  }
);

setMethod(  "Quantity", "Portfolio", 
  function( .Object, asOfDate = today() ) {
    qty = matrix( .Object@Quantity, ncol = 1 );
    rownames(qty) = AssetSymbols( .Object );
    colnames(qty) = 'Quantity';
    return(qty);
  }
);

setGeneric( "Simulation", def=function( .Object ) standardGeneric( "Simulation" ) );
setMethod(  "Simulation", "Portfolio", function( .Object ) .Object@Simulation );

setGeneric( "SimulateAssetReturns", def=function( .Object ) standardGeneric( "SimulateAssetReturns" ) );
setMethod(  "SimulateAssetReturns", "Portfolio", function( .Object ) { 
    sim.rtns = Simulation( .Object )@Returns;
    if( is.empty( sim.rtns ) ) {
       stop( 'Simulation object is empty: you must call setSimulation to initialize the simulation');
    } else {
      return( sim.rtns );
    }
  }
);

setGeneric( "SimulateTradeReturns", def=function( .Object ) standardGeneric( "SimulateTradeReturns" ) );
setMethod(  "SimulateTradeReturns", "Portfolio", function( .Object ) {
    sim.rtns = SimulateAssetReturns( .Object );
    trade.asset.map = TradeToAssetMap( .Object );
    sim.trade.rtns = sim.rtns %*% t( trade.asset.map );
    return( sim.trade.rtns );
  }
);

setGeneric( "AssetWeights", def=function( .Object ) standardGeneric( "AssetWeights" ) );
setMethod(  "AssetWeights", "Portfolio",
  function( .Object ) {
    asset.weights = MktVal( .Object ) / sum( MktVal( .Object ) );
    colnames( asset.weights ) = 'AssetWeights';
    return( asset.weights );
  }
);

setGeneric( "TradeToAssetMap", def=function( .Object ) standardGeneric( "TradeToAssetMap" ) );
setMethod(  "TradeToAssetMap", "Portfolio", 
  function( .Object ) {
    trade.asset.mv = TradeQuantity(.Object) %*% diag( as.numeric( AssetPrices(.Object) ) );
    colnames( trade.asset.mv ) = colnames( TradeQuantity( .Object ) );

    trade.asset.map = diag( 1/as.numeric( TradeMktVal(.Object) + 1e-10 ) ) %*% trade.asset.mv;
    rownames( trade.asset.map ) = rownames( trade.asset.mv );

    # Add any missing columns for Asset symbols contained in the portfolio
    trade.asset.map <- add.df.columns( trade.asset.map, labels = Symbols(.Object), fill.value = 0 );
    trade.asset.map <- as.matrix( trade.asset.map[,Symbols(.Object)] );
    return( trade.asset.map );
  }
);

setGeneric( "AssetToTradeMap", def=function( .Object ) standardGeneric( "AssetToTradeMap" ) );
setMethod(  "AssetToTradeMap", "Portfolio",
  function( .Object ) {
    # Start with Trade to Asset map
    trade.asset.map = TradeToAssetMap( .Object );
    # Find the market values of assets within each trade
    wts = TradeQuantity(.Object) %*% AssetPrices(.Object);
    wtd.map = diag( as.numeric( TradeWeights( .Object ) ) ) %*% trade.asset.map;
    # Find the proportion of each asset's total weight in the trades
    asset.trade.map =  t( wtd.map %*%  diag(1/as.numeric( AssetWeights(.Object)) ) );
    rownames( asset.trade.map ) = colnames( trade.asset.map );
    colnames( asset.trade.map ) = rownames( trade.asset.map );
    return( asset.trade.map );
  }
);

setGeneric( "UnderlierWeights", def=function( .Object ) standardGeneric( "UnderlierWeights" ) );
setMethod(  "UnderlierWeights", "Portfolio", 
	function( .Object ) {
	  underliers = UnderlierSymbols( .Object );
          weights = Leverage( .Object ) * AssetWeights( .Object ); 
          underlier.wts= matrix( NaN, ncol = 1, nrow = length( underliers ) );
          for( i in 1:length( underliers ) ) {
            underlier.wts[i] = sum( weights[ OptionInfo( .Object )$Underlier == underliers[i] ] );
          };
         
          rownames( underlier.wts ) = UnderlierSymbols( .Object ); 
	  colnames( underlier.wts ) = 'UnderlierWeights';
	  return( underlier.wts );
	}
);

setGeneric( "UnderlierSymbols", def=function( .Object ) standardGeneric( "UnderlierSymbols" ) );
setMethod(  "UnderlierSymbols", "Portfolio", 
	function( .Object ) { 
	  return( unique( OptionInfo( .Object )$Underlier ) );
	}
);

setGeneric( "AssetBetas", def=function( .Object, factor.list = .Object@FactorList, freq = 'D', return.all = FALSE )
                                                                                                standardGeneric( "AssetBetas" ) );
setMethod(  "AssetBetas", "Portfolio", 
  function( .Object, factor.list = .Object@FactorList, freq = 'D', return.all ) {
    underlier.betas = UnderlierBetas( .Object, factor.list = factor.list, freq = freq, return.all = return.all );
    option.symbols = .Object@Symbols[  is.option.symbol( .Object@Symbols ) ];
    fx.fwd.symbols = .Object@Symbols[  is.fx.forward.symbol( .Object@Symbols ) ];
    deriv.symbols = c( option.symbols, fx.fwd.symbols );

    deriv.info = OptionInfo( .Object );
    factor.names = get.factor.names( factor.list );
   
    if( !is.empty( deriv.symbols ) ) { 
      for( i in 1:length( deriv.symbols ) ) {
        deriv.info = OptionInfo( .Object )[ deriv.symbols[i], ];
        option.betas = data.frame( underlier.betas[ deriv.info$Underlier, ] );
        colnames( option.betas ) = colnames( underlier.betas );
        rownames( option.betas ) = deriv.symbols[i];
        option.betas[ , factor.names ] = option.betas[ , factor.names ] * deriv.info$Leverage;
        underlier.betas = rbind( underlier.betas, option.betas );
      };
    };

    asset.betas = data.frame( underlier.betas[ AssetSymbols( .Object ), ] );
    colnames( asset.betas ) = colnames( underlier.betas );
    rownames( asset.betas ) = AssetSymbols( .Object );
    return( asset.betas );
  }
);

setGeneric( "UnderlierBetas", def=function( .Object, factor.list = .Object@FactorList, freq = 'D', return.all = FALSE )
                                								standardGeneric( "UnderlierBetas" ) );
setMethod(  "UnderlierBetas", "Portfolio", function( .Object, factor.list = .Object@FactorList, freq = 'D', return.all = FALSE ) {
    betas = get.beta( UnderlierSymbols( .Object ), factor.list = factor.list, freq = freq, return.all = return.all );
    return( betas );
  }
);

setGeneric( "changeMktVal", def = function( .Object, amount = c(), from = .Object@BaseCurrency, to = .Object@BaseCurrency ) 
											standardGeneric( 'changeMktVal' ) );
setMethod(  "changeMktVal", "Portfolio", function( .Object, amount = c(), from = .Object@BaseCurrency, to = .Object@BaseCurrency ) {
     mktval = MktVal( .Object );
     symbols = AssetSymbols( .Object );
  
     if( length( from ) > 1 && length( to ) == 1 ) {
       to = rep( to, length( from ) );
     } else if( length( to ) > 1 && length( from ) == 1 ) {
       from = rep( from, length( to ) );
     };

     if( is.null( amount ) ) {
       amount = mktval[ match( from, symbols ) ];
     };

     for( i in 1:length( from ) ) {
       if( from[i] %in% symbols && to[i] %in% symbols ) {
         mktval[ symbols == from[i] ] = mktval[ symbols == from[i] ] - amount[i];
         mktval[ symbols == to[i]   ] = mktval[ symbols == to[i]   ] + amount[i];
       };
     };

    .Object@MktVal = mktval;
    return( .Object );
  }
);

setGeneric( "addAssets", def = function( .Object, symbols ) standardGeneric( 'addAssets' ) );
setMethod(  "addAssets", "Portfolio", 
  function( .Object, symbols ) {
    new.symbols = setdiff( symbols, Symbols(.Object) );

    .Object@Symbols = c( .Object@Symbols, new.symbols );

    # Append 0 to the MktVal and Quantity
    .Object@MktVal   = rbind( .Object@MktVal,   matrix( rep( 0, length( new.symbols ) ), ncol = 1 ) );
    .Object@Quantity = rbind( .Object@Quantity, matrix( rep( 0, length( new.symbols ) ), ncol = 1 ) );

    # Update the OptionInfo
    for( i in 1:length(new.symbols) ) {
      .Object@OptionInfo = rbind( .Object@OptionInfo, getOptionInfoSingleAsset( .Object, new.symbols[i] ) );
    };

    return( .Object );
  }
);

setGeneric( "Beta", def=function( .Object, factor.list = .Object@FactorList, freq = 'D', return.all = FALSE ) standardGeneric( "Beta" ) );
setMethod(  "Beta", "Portfolio", function( .Object, factor.list = .Object@FactorList, freq = 'D', return.all = FALSE ) {
    betas = AssetBetas( .Object, factor.list = factor.list, freq = freq )
    pf.beta = t( t( betas ) %*% AssetWeights( .Object ) );
    return( pf.beta );
  }
);

setGeneric( "MarginEquityRatio", def=function( .Object, factor.list = .Object@FactorList, freq = 'D' ) 
									standardGeneric( "MarginEquityRatio" ) );
setMethod(  "MarginEquityRatio", "Portfolio", function( .Object ) {
    margin.equity = calc.margin.equity.ratio( UnderlierWeights( .Object ), UnderlierSymbols( .Object ) );
    return( margin.equity );
  }
);

setGeneric( "Volatility", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "Volatility" ) );

setMethod(  "Volatility", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) {
    vol = tryCatch( calc.pf.vol( UnderlierSymbols( .Object ), UnderlierWeights( .Object ), method = method, freq = freq,
                                start.date = start.date, end.date = end.date ), error = function(e) NA );
    return( as.numeric(vol) );
  }
);

setGeneric( "UnderlierVolatility", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "UnderlierVolatility" ) );

setMethod(  "UnderlierVolatility", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    covar = UnderlierCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    vols = t( t( sqrt( diag( covar ) ) ) );
    colnames( vols ) = 'Volatility';
    return( vols );
  }
);

setGeneric( "AssetVolatility", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "AssetVolatility" ) );

setMethod(  "AssetVolatility", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    covar = AssetCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    vols = t( t( sqrt( diag( covar ) ) ) );
    colnames( vols ) = 'Volatility';
    return( vols );
  }
);

setGeneric( "TradeVolatility", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "TradeVolatility" ) );

setMethod(  "TradeVolatility", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    covar = TradeCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    vols = t( t( sqrt( diag( covar ) ) ) );
    colnames( vols ) = 'Volatility';
    return( vols );
  }
);

setGeneric( "UnderlierCovariance", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30), 
						end.date = last.biz.date() ) standardGeneric( "UnderlierCovariance" ) );

setMethod(  "UnderlierCovariance", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D', 
						start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    covar = tryCatch( calc.pf.cov( UnderlierSymbols( .Object ), method = method, freq = freq, 
				start.date = start.date, end.date = end.date ), error = function(e) NA );
    return( covar );
  }
);

setGeneric( "UnderlierCorrelation", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "UnderlierCorrelation" ) );

setMethod(  "UnderlierCorrelation", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D', 
						start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    covar = UnderlierCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    correl = tryCatch( cov2cor( covar ) );
    return( correl );
  }
);

setGeneric( "UnderlierRiskAttribution", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "UnderlierRiskAttribution" ) );

setMethod(  "UnderlierRiskAttribution", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D', 
							start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    wts = UnderlierWeights(.Object);
    covar = UnderlierCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    pf.var = as.numeric( t( wts ) %*% covar %*% wts );
    risks = tryCatch( t( t(  wts ) %*% covar ) * wts / pf.var  * 100 );
    return( risks );
  }
);


setGeneric( "AssetCovariance", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "AssetCovariance" ) );

setMethod(  "AssetCovariance", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    underlier.covar = UnderlierCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    weights = AssetToUnderlierMap( .Object );
    covar = weights %*% underlier.covar %*% t(weights);
    return( covar );
  }
);

setGeneric( "AssetCorrelation", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "AssetCorrelation" ) );

setMethod(  "AssetCorrelation", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    covar = AssetCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    correl = tryCatch( cov2cor( covar ) );
    return( correl );
  }
);

setGeneric( "AssetRiskAttribution", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "AssetRiskAttribution" ) );

setMethod(  "AssetRiskAttribution", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                        start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    wts = AssetWeights(.Object);
    covar = AssetCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    pf.var = as.numeric( t( wts ) %*% covar %*% wts );
    risks = tryCatch( t( t(  wts ) %*% covar ) * wts / pf.var  * 100 );
    return( risks );
  }
);

setGeneric( "AssetIncrementalCVaR", def=function( .Object, VaR.pct = 0.05 ) standardGeneric( "AssetIncrementalCVaR" ) );

setMethod(  "AssetIncrementalCVaR", "Portfolio", function( .Object, VaR.pct = 0.05 ) {
    asset.mktval = MktVal( .Object );

    tot.mv = sum(asset.mktval);
    pf.cvar = CVaR( .Object, VaR.pct = VaR.pct );

    incremental.cvar = c();
    tot.wt = c();
    for( i in 1:length( asset.mktval ) ) {
      .Object@MktVal = asset.mktval;
      .Object@MktVal[i] = 0;
      mv = sum(MktVal(.Object));

       incremental.cvar[i] = pf.cvar - mv/tot.mv * CVaR( .Object, VaR.pct = VaR.pct );
    };

    names( incremental.cvar ) = AssetSymbols( .Object ); 
    incremental.cvar = t( t( incremental.cvar ) );
    return( incremental.cvar );
  }
);

setGeneric( "AssetIncrementalVaR", def=function( .Object, VaR.pct = 0.05 ) standardGeneric( "AssetIncrementalVaR" ) );

setMethod(  "AssetIncrementalVaR", "Portfolio", function( .Object, VaR.pct = 0.05 ) {
    asset.mktval = AssetWeights( .Object );

    tot.mv = sum(asset.mktval);
    pf.var = VaR( .Object, VaR.pct = VaR.pct );

    incremental.var = c();
    for( i in 1:length( asset.mktval ) ) {
      .Object@MktVal = asset.mktval;
      .Object@MktVal[i] = 0;
      mv = sum( MktVal(.Object));

       incremental.var[i] = pf.var - mv/tot.mv * VaR( .Object, VaR.pct = VaR.pct );
    };

    names( incremental.var ) = AssetSymbols( .Object );
    incremental.var = t( t( incremental.var ) );
    return( incremental.var );
  }
);

setGeneric( "TradeIncrementalCVaR", def=function( .Object, VaR.pct = 0.05 ) standardGeneric( "TradeIncrementalCVaR" ) );

setMethod(  "TradeIncrementalCVaR", "Portfolio", function( .Object, VaR.pct = 0.05 ) {
    asset.mktval = MktVal( .Object );

    tot.mv = sum( MktVal(.Object) );
    pf.cvar = CVaR( .Object, VaR.pct = VaR.pct );
    trade.map = AssetToTradeMap( .Object );

    incremental.cvar = c();
    for( i in 1:ncol( trade.map ) ) {
      .Object@MktVal = asset.mktval * ( trade.map[,i] == 0 );
      mv = sum(MktVal(.Object));

       incremental.cvar[i] = pf.cvar - mv/tot.mv * CVaR( .Object, VaR.pct = VaR.pct );
    };

    names( incremental.cvar ) = TradeSymbols( .Object );
    incremental.cvar = t( t( incremental.cvar ) );
    return( incremental.cvar );
  }
);

setGeneric( "TradeIncrementalVaR", def=function( .Object, VaR.pct = 0.05 ) standardGeneric( "TradeIncrementalVaR" ) );

setMethod(  "TradeIncrementalVaR", "Portfolio", function( .Object, VaR.pct = 0.05 ) {
    asset.mktval = MktVal( .Object );

    tot.mv = sum( MktVal(.Object) );
    pf.var = VaR( .Object, VaR.pct = VaR.pct );
    trade.map = AssetToTradeMap( .Object );

    incremental.var = c();
    for( i in 1:ncol( trade.map ) ) {
      .Object@MktVal = asset.mktval * ( trade.map[,i] == 0 );
      mv = sum(MktVal(.Object));

       incremental.var[i] = pf.var - mv/tot.mv * VaR( .Object, VaR.pct = VaR.pct );
    };

    names( incremental.var ) = TradeSymbols( .Object );
    incremental.var = t( t( incremental.var ) );
    return( incremental.var );
  }
);

setGeneric( "TradeRiskSummary", def=function( .Object ) standardGeneric( "TradeRiskSummary" ) );

setMethod(  "TradeRiskSummary", "Portfolio", function( .Object ) {
    MV  = sum( MktVal( .Object ) );
    vol = Volatility( .Object );
    risk.var = TradeRiskAttribution( .Object )/100;
    cvar.95  = TradeIncrementalCVaR( .Object, VaR.pct = .05 );
    cvar.99  = TradeIncrementalCVaR( .Object, VaR.pct = .01 );

    # Combine Risk Data
    summary = cbind( risk.var * vol * MV, risk.var * vol,
		     cvar.95 * MV,  cvar.95, 
		     cvar.99 * MV,  cvar.99 );
    colnames( summary ) = c( 'Vol ($)', 'Vol (%)', 'CVaR 95% ($)', 'CVaR 95% (%)', 'CVaR 99% ($)', 'CVaR 99% (%)' );

    # Add Total Row
    total.row = data.frame( MV*vol, vol, MV*CVaR(.Object,.05), CVaR(.Object,.05), MV*CVaR(.Object,.01), CVaR(.Object,.01) );
    rownames( total.row ) = 'Total';
    colnames( total.row ) = colnames( summary );
 
    # Combine Total row with summary data
    summary = rbind( total.row, summary );

    # Format the output
    summary[,1] = formatC( summary[,1], digits = 0, format = 'd', big.mark = ',' );
    summary[,2] = formatC( 100*summary[,2], digits = 1, format = 'f' );
    summary[,3] = formatC( summary[,3], digits = 0, format = 'd', big.mark = ',' );
    summary[,4] = formatC( 100*summary[,4], digits = 1, format = 'f' );
    summary[,5] = formatC( summary[,5], digits = 0, format = 'd', big.mark = ',' );
    summary[,6] = formatC( 100*summary[,6], digits = 1, format = 'f' );
   
    # Format zero entries to be dashes
    summary[ summary == '0' ] = '-';
    summary[ summary == '0.0' ] = '-';
 
    return( summary );
  }
);

setGeneric( "TradeCovariance", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "TradeCovariance" ) );

setMethod(  "TradeCovariance", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    asset.covar = AssetCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    asset.to.trade = t(TradeToAssetMap( .Object ));
    covar = t(asset.to.trade) %*% asset.covar %*% asset.to.trade;
    return( covar );
  }
);

setGeneric( "TradeCorrelation", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "TradeCorrelation" ) );

setMethod(  "TradeCorrelation", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    covar = TradeCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    correl = tryCatch( cov2cor( covar ) );
    return( correl );
  }
);

setGeneric( "TradeRiskAttribution", def=function( .Object, method = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30),
                                                end.date = last.biz.date() ) standardGeneric( "TradeRiskAttribution" ) );

setMethod(  "TradeRiskAttribution", "Portfolio", function( .Object, method = .Object@FactorList, freq = 'D',
                                                        start.date = today() %m-% years(30), end.date = last.biz.date() ) {
    wts = TradeWeights(.Object);
    covar = TradeCovariance( .Object, method = method, freq = freq, start.date = start.date, end.date = end.date );
    pf.var = as.numeric( t( wts ) %*% covar %*% wts );
    risks = tryCatch( t( t(  wts ) %*% covar ) * wts / pf.var  * 100 );
    return( risks );
  }
);

setGeneric( "UnderlierRiskPremia", def=function( .Object, factor.list = .Object@FactorList, freq = 'D',  
	start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) standardGeneric( "UnderlierRiskPremia" ) );
setMethod(  "UnderlierRiskPremia", "Portfolio", function( .Object, factor.list=.Object@FactorList, freq='D', 
	start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) {

    underlier.premia = calc.asset.premia( UnderlierSymbols( .Object ), factor.list = factor.list, freq = freq, 
                                            start.date = start.date, end.date = end.date, use.etf.alpha = use.etf.alpha );
    return( underlier.premia );
  }
);

setGeneric( "AssetRiskPremia", def=function( .Object, factor.list = .Object@FactorList, freq = 'D',
        start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) standardGeneric( "AssetRiskPremia" ) );
setMethod(  "AssetRiskPremia", "Portfolio", function( .Object, factor.list=.Object@FactorList, freq='D',
        start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) {

    underlier.premia = UnderlierRiskPremia( .Object, factor.list=.Object@FactorList, freq='D',
        				start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T );
    weights = AssetToUnderlierMap( .Object );
    asset.risk.premia = weights %*% underlier.premia;
    
    return( asset.risk.premia );
  }
);

setGeneric( "TradeRiskPremia", def=function( .Object, factor.list = .Object@FactorList, freq = 'D',
        start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) standardGeneric( "TradeRiskPremia" ) );
setMethod(  "TradeRiskPremia", "Portfolio", function( .Object, factor.list=.Object@FactorList, freq='D',
        start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) {

    asset.risk.premia = AssetRiskPremia( .Object, factor.list=.Object@FactorList, freq='D',
                                        start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T );
    weights = TradeToAssetMap( .Object );
    trade.risk.premia = weights %*% asset.risk.premia;

    return( trade.risk.premia );
  }
);


setGeneric( "RiskPremia", def=function( .Object, factor.list = .Object@FactorList, freq = 'D', 
	start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) standardGeneric( "RiskPremia" ) );
setMethod(  "RiskPremia", "Portfolio", function( .Object, factor.list=.Object@FactorList, freq='D', 
	start.date = today() %m-% years(30), end.date = last.biz.date(), use.etf.alpha = T ) {

    underlier.risk.premia = UnderlierRiskPremia( .Object, factor.list = factor.list, start.date = start.date, end.date = end.date, 
									freq = freq, use.etf.alpha = use.etf.alpha );

    premia = as.numeric( t( UnderlierWeights( .Object ) ) %*% underlier.risk.premia  );

    return( premia );
  }
);

setGeneric( "Sharpe", def=function( .Object, factor.list = .Object@FactorList, freq = 'D' ) standardGeneric( "Sharpe" ) );
setMethod(  "Sharpe", "Portfolio", function( .Object, factor.list = .Object@FactorList, freq = 'D' ) {
    SR = RiskPremia( .Object ) / Volatility( .Object );
    return( SR );
  }
);

setGeneric( "SimulateMktVal", def=function( .Object  ) standardGeneric( "SimulateMktVal" ) );
setMethod(  "SimulateMktVal", "Portfolio", function( .Object ) {
    sim.mv = apply( SimulateAssetWeights( .Object ), 1, sum );
    return( sim.mv );
  }
);

setGeneric( "SimulateAssetWeights", def=function( .Object  ) standardGeneric( "SimulateAssetWeights" ) );
setMethod(  "SimulateAssetWeights", "Portfolio", function( .Object ) {
    asset.rtns = SimulateAssetReturns( .Object );
    wts = AssetWeights( .Object );
    sim.wts = ( 1 + asset.rtns ) %*% diag( as.numeric( wts ) );
    return( sim.wts );
  }
);

setGeneric( "SimulateTradeWeights", def=function( .Object  ) standardGeneric( "SimulateTradeWeights" ) );
setMethod(  "SimulateTradeWeights", "Portfolio", function( .Object ) {
    trade.rtns = SimulateTradeReturns( .Object );
    wts = TradeWeights( .Object );
    sim.wts = ( 1 + trade.rtns ) %*% diag( as.numeric( wts ) );
    return( sim.wts );
  }
);

setGeneric( "SimulateBetas", def=function( .Object ) standardGeneric( "SimulateBetas" ) );
setMethod(  "SimulateBetas", "Portfolio", function( .Object ) {
    factor.names = get.factor.names( Simulation( .Object )@FactorList );
    sim.betas = matrix( NaN, nrow = Simulation( .Object )@NumberOfSimulations, ncol = length( factor.names ) );
    colnames( sim.betas ) = factor.names;

    asset.rtns = SimulateAssetReturns( .Object );

    for( i in 1:length( factor.names ) ) {
      sim.wts = SimulateAssetWeights( .Object );
      asset.betas = sim.wts %*% diag( AssetBetas( .Object )[ ,factor.names[i] ]);
      sim.betas[,i] = apply( asset.betas, 1, sum );
    };

    return( sim.betas );
  }
);

setGeneric( "SimulateMarginEquityRatio", def=function( .Object ) standardGeneric( "SimulateMarginEquityRatio" ) );
setMethod(  "SimulateMarginEquityRatio", "Portfolio", function( .Object ) {
    margin.equity.ratio = calc.margin.equity.ratio(  SimulateAssetWeights( .Object ), AssetSymbols( .Object ) );
    return( margin.equity.ratio );
  }
);


setGeneric( "VaR", def=function( .Object, VaR.pct = 0.05 ) standardGeneric( "VaR" ) );
setMethod(  "VaR", "Portfolio", function( .Object, VaR.pct = .05 ) {
    sim.wts = SimulateMktVal( .Object );
    tot.wts = sum( AssetWeights( .Object ) );
    pf.var = tot.wts - quantile( sim.wts, VaR.pct );
    return( pf.var );
  }
);

setGeneric( "CVaR", def=function( .Object, VaR.pct = 0.05 ) standardGeneric( "CVaR" ) );
setMethod(  "CVaR", "Portfolio", function( .Object, VaR.pct = .05 ) {
    sim.wts = SimulateMktVal( .Object );
    valueAtRisk = VaR( .Object, VaR.pct = VaR.pct );

    tot.wts = sum( AssetWeights( .Object ) );
    pf.cvar = -unlist( lapply( -valueAtRisk, function(x) mean( -tot.wts + sim.wts[ sim.wts - tot.wts <= x ] ) ) );

    return( pf.cvar );
  }
);

setGeneric( "HistoricalTS", def=function( .Object, factor.list = .Object@FactorList, freq = 'D', start.date = today() %m-% years(30), 
		end.date = last.biz.date(), rebalance.period = 30 )  standardGeneric( "HistoricalTS" ) ); 
setMethod(  "HistoricalTS", "Portfolio", 
  function( 
    .Object, 
    factor.list = .Object@FactorList, 
    freq = 'D', 
    start.date = today() %m-% years(30),
    end.date = last.biz.date(), 
    rebalance.period = 30 
  ) {
    # Create proxy total return series for each of the underliers
    rtns = proxy.from.factors( UnderlierSymbols( .Object ), factor.list = factor.list, start.date = start.date, 
								end.date = end.date, freq = freq, totalReturn = TRUE );
    # Calculate the normalized weights
    target.weights = as.numeric( UnderlierWeights( .Object ) / sum( UnderlierWeights( .Object ) ) );

    # Calculate the performance of a strategy that rebalances to the target weights every 'rebalance.period' days.
    ts = portfolio.returns( prod.ts( rtns ), weights = target.weights, start.date = start.date, 
					end.date = end.date, rebalance.period = rebalance.period );
    return( ts );
  }
);

###########################################################################################
# Routine: Portfolio::print.Portfolio
###########################################################################################

setGeneric( 'print.Portfolio', def=function( .Object ) standardGeneric( 'print.Portfolio' ) );
setMethod( 'print.Portfolio', 'Portfolio', 
  function( .Object ) {
    wts = t( t( round( AssetWeights( .Object ) * 10000 ) / 100 ) );
    colnames( wts ) = 'Weights';
    print( wts );
  }
);

###########################################################################################
# Routine: Portfolio::HistoricalDrawdowns
###########################################################################################

setGeneric( 'HistoricalDrawdowns', def=function( .Object ) standardGeneric( 'HistoricalDrawdowns' ) );
setMethod( 'HistoricalDrawdowns', 'Portfolio',
  function( .Object ) {
    hist.ts = HistoricalTS( .Object )
    tot = hist.ts[,'Total'];
    drawdowns = get.historical.drawdowns( tot );

    # Format the output
    drawdowns[,1] = format( drawdowns[,1], '%m/%d/%y' )
    drawdowns[,2] = format( drawdowns[,2], '%m/%d/%y' )
    drawdowns[,3] = format( drawdowns[,3], '%m/%d/%y' )
    drawdowns[,4] = formatC( 100*drawdowns[,4], format = 'f', digits = 1 );

    return( drawdowns );
  }
);

###########################################################################################
# Routine: Portfolio::setFactorList
###########################################################################################

setGeneric( "setFactorList", def=function( .Object, factor.list ) standardGeneric( "setFactorList" ) );
setMethod(  "setFactorList", "Portfolio",
  function( .Object, factor.list ) {
    .Object@FactorList = factor.list;
    return( .Object );
  }
);

###########################################################################################
# Routine: Portfolio::setSimulation
###########################################################################################

setGeneric( "setSimulation", def=function( .Object, factor.list = .Object@FactorList ) standardGeneric( "setSimulation" ) );

setMethod(  "setSimulation", "Portfolio",
  function( .Object, factor.list = .Object@FactorList ) {

    .Object@Simulation@Symbols = .Object@Symbols;
    .Object@Simulation@FactorList = factor.list;

    .Object@Simulation = setReturns( .Object@Simulation );

    return( .Object );
  }
);

###########################################################################################
# Routine: Portfolio::NotionalMktVal
###########################################################################################

setGeneric( "NotionalMktVal", def=function( .Object ) standardGeneric( "NotionalMktVal" ) );
setMethod(  "NotionalMktVal", "Portfolio",
  function( .Object ) {
     notional = MktVal( .Object );

     ac.list = get.asset.class.list( AssetSymbols( .Object ) );

     if( !is.empty( ac.list$CurrencyForward ) ) {
       fwd.symbols = ac.list$CurrencyForward;
       for( fwd.symbol in fwd.symbols ) {
         fwd.info = parse.fx.forward.symbols( fwd.symbol );
         idx = fwd.symbol == AssetSymbols( .Object );
         qty = Quantity( .Object )[ idx ];
         if( fwd.info$Long.Currency == BaseCurrency( .Object ) ) {
           notional[idx] = qty;
         } else if( fwd.info$Short.Currency == BaseCurrency( .Object ) ) {
           notional[idx] = convert.currency( qty, from.ccy = fwd.info$Long.Currency, 
			to.ccy = BaseCurrency( .Object ), asOfDate = AsOfDate( .Object ) );
         } else {
           stop( sprintf( 'Unsupported currency pair: %s', fwd.info$Currency.Cross ) );
         };
       };
     };

     if( !is.empty( ac.list$Option ) ) {
       opt.info = parse.option.symbols( ac.list$Option );

       direction = 1;
       direction[ opt.info$Type == 'P' ] = -1;

       opt.qty = Quantity( .Object )[ match( opt.info$Option.Symbol, AssetSymbols( .Object ) ) ];
       SD = AsOfDate( .Object ) - 21;
       ED = AsOfDate( .Object ) + 21;
       uniq.und.sym = unique( opt.info$Symbol );
       und.prc.ts = fill.missing( get.time.series( uniq.und.sym, start.date = SD, end.date = ED ) );
       und.prc = und.prc.ts[ AsOfDate( .Object ), ];
       prc = und.prc[ match( opt.info$Symbol, colnames( und.prc ) ) ]; 
       opt.notional = opt.qty * prc * direction * opt.info$Strike;
       notional[ match( opt.info$Option.Symbol, AssetSymbols( .Object ) ) ] = opt.notional;
     };

     colnames( notional ) = 'NotionalMktVal';
     return(notional);
  }
);

