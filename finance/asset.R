
###########################################################################################
# Routine: get.asset.class
###########################################################################################

get.asset.class <- function( symbols ) {

  # Find indicators for different asset classets
  is.cash     = is.cash.symbol( symbols );
  is.equity   = is.equity.symbol( symbols );
  is.index    = is.index.symbol( symbols );
  is.option   = is.option.symbol( symbols );
  is.ccy      = is.currency.symbol( symbols );
  is.ccy.pair = is.currency.pair.symbol( symbols );
  is.fx.fwd   = is.fx.forward.symbol( symbols );

  # Create output array with asset class names
  output = character(length(symbols));
  output[ is.cash ]     = 'Cash';
  output[ is.equity ]   = 'Equity';	
  output[ is.index ]    = 'Index';	
  output[ is.option ]   = 'Option';
  output[ is.ccy ]      = 'Currency';
  output[ is.ccy.pair ] = 'CurrencyPair';
  output[ is.fx.fwd ]   = 'CurrencyForward';

  return( output );
};

###########################################################################################
# Routine: get.asset.class.list
###########################################################################################

get.asset.class.list <- function( symbols ) {
  result = get.asset.class( symbols );

  output = list();
  output$Cash            = symbols[ result == 'Cash' ];
  output$Equity          = symbols[ result == 'Equity' ];
  output$Index           = symbols[ result == 'Index' ];
  output$Option          = symbols[ result == 'Option' ];
  output$Currency        = symbols[ result == 'Currency' ];
  output$CurrencyPair    = symbols[ result == 'CurrencyPair' ];
  output$CurrencyForward = symbols[ result == 'CurrencyForward' ];
  output$Other           = symbols[ result == '' ];

  # Sort the non-empty asset class lists
  for( j in 1:length(output) ) {
    res = output[[j]];
    if( length(res) ) { 
      output[[j]] = sort(res);
    } else {};
  };

  # Return the sorted list
  return( output );
};

###########################################################################################
# Routine: is.cash.symbol
###########################################################################################

is.cash.symbol <- function( symbols ) {
  currency.info = read.table( EXCHANGE_RATE_INFO_CSV, sep = ',', header = TRUE, stringsAsFactor = FALSE );
  currency.symbols = c( 'USD', currency.info$Currency );
  cash.symbols = paste( currency.symbols, 'Cash', sep = '' );

  result = ( symbols %in% cash.symbols );
  return( result );
};

###########################################################################################
# Routine: parse.cash.symbol
###########################################################################################

parse.cash.symbol <- function( cash.symbols ) {
  ccy = gsub( 'Cash', '', cash.symbols );
  ccy[ !is.cash.symbol( cash.symbols ) ] = '';
  info = data.frame( Symbol = cash.symbols, Currency = ccy );
  return( info );
};

###########################################################################################
# Routine: is.currency.symbol
###########################################################################################

is.currency.symbol <- function( symbols ) {
  currency.info = read.table( EXCHANGE_RATE_INFO_CSV, sep = ',', header = TRUE, stringsAsFactor = FALSE );
  currency.list = c( 'USD', currency.info$Currency );

  result = ( symbols %in% currency.list );
  return( result );
};

###########################################################################################
# Routine: is.currency.pair.symbol
###########################################################################################
 
is.currency.pair.symbol <- function( symbols ) {
  currency.info = read.table( EXCHANGE_RATE_INFO_CSV, sep = ',', header = TRUE, stringsAsFactor = FALSE );
  currency.list = c( 'USD', currency.info$Currency );

  parsed = strsplit( symbols, '/' );
 
  result = logical(length(parsed));
  for( i in 1:length( parsed ) ) {
    elem = parsed[[i]];
    if( length(elem) == 2 ) {
      if( all( elem %in% currency.list ) ) {
        result[i] = TRUE;
      };
    };
  };

  return( result );
};

###########################################################################################
# Routine: parse.currency.pair.symbols
###########################################################################################

parse.currency.pair.symbols <- function( symbols ) {
  parsed = strsplit( symbols, '/' );
  output = c();
  for( i in 1:length(parsed) ) {
    output = rbind( output, data.frame( Long.Currency = parsed[[i]][2], Short.Currency = parsed[[i]][1] ) );
  };

  return(output);
};
 
###########################################################################################
# Routine: is.equity.symbol
###########################################################################################

is.equity.symbol <- function( symbols ) {
  equity.symbols = get.all.equity.symbols();
  result = symbols %in% equity.symbols;
  return( result );
};

###########################################################################################
# Routine: parse.equity.symbols
###########################################################################################

parse.equity.symbols <- function( equity.symbols ) {

  # Create output data frame
  col = rep( '', length( equity.symbols ) );
  output = data.frame( Symbol = equity.symbols, Name = col, Exchange = col, Currency = col );

  # Gety.symbols, exchange.listings$Symbol )t the list of all equities and their exchanges
  equity.listings = get.equity.exchange.listing.info()
  
  # Try to figure out which exchange given stocks are listed on from their suffix
  symbol.info = get.equity.exchange.info.from.suffix( equity.symbols );
 
  # Match the equity symbols to those for which we have exchange info
  idx = match( equity.symbols, equity.listings$Symbol );

  # For those symbol where we can't find info, try to remove the suffix for US equities and try again
  idx[ is.na(idx) ] = match( remove.US.exchange.suffix( equity.symbols[is.na(idx)] ), 
			     remove.US.exchange.suffix( equity.listings$Symbol ) );

  # Populate the data frame with the relevant information
  output[ !is.na(idx), c( 'Name', 'Exchange', 'Currency' ) ] = 
		equity.listings[ idx[ !is.na(idx) ], c( 'Description', 'Exchange', 'Currency' ) ];

  # For tickers that weren't found, try to at least get some information from their exchange suffix
  missing.idx = ( output$Currency == '' );
  exchange.info = get.equity.exchange.info.from.suffix( equity.symbols[ missing.idx ] );
  output[ missing.idx, c( 'Exchange', 'Currency' ) ] = exchange.info[ , c( 'Exchange', 'Currency' ) ];

  return( output );
};

###########################################################################################
# Routine: is.index.symbol
###########################################################################################

is.index.symbol <- function( symbols ) {
  result = ( 1 == regexpr( '^\\^', symbols ) );
  return( result );
};

###########################################################################################
# Routine: is.etf.symbol
###########################################################################################

is.etf.symbol <- function( symbols ) {
  stop( 'Not currently supported' );
  return( result );
};

###########################################################################################
# Routine: parse.etf.symbols
# 
# Return information for all ETFs/Indexes in the .csv file
###########################################################################################
    
parse.etf.symbols <- function( Symbols = NULL ) { 
  all.etf.info <- read.csv( ETF_INFORMATION_CSV, sep=",", header=TRUE, stringsAsFactors = FALSE );

  if( is.empty( Symbols ) ) {
    output = all.etf.info;
  } else {
    asset.row.locs <- match( Symbols, all.etf.info$Symbol );

    if( length( asset.row.locs ) == 0 ) {
      output = data.frame();
    } else {
      output =  all.etf.info[ asset.row.locs, ];
    };  
  };
  
  return( output );
};

###########################################################################################
# Routine: is.fx.forward.symbol
###########################################################################################

is.fx.forward.symbol <- function( symbols ) {
  result = substr( symbols, 1, 8 ) == 'Forward_';
  return( result );
};

###########################################################################################
# Routine: parse.fx.forward.symbols
###########################################################################################

parse.fx.forward.symbols <- function( symbols ) {

  from.ccy = substr( symbols, 9, 11 );
  to.ccy = substr( symbols, 12, 14 );
  
  settlement.dates = character(length(symbols));
  settlement.rates = numeric(length(symbols));
  for( i in 1:length( symbols ) ) {
    Y = substr( symbols[i], 15, 18 );
    M = substr( symbols[i], 19, 20 );
    D = substr( symbols[i], 21, 22 );
    settlement.dates[i] = sprintf( '%s-%s-%s', Y, M, D );

    settlement.rates[i] = as.numeric(strsplit( symbols[i], '_' )[[1]][3]);
  };

  output = data.frame( Symbol = symbols, Currency.Cross = paste( from.ccy, to.ccy, sep = '/' ), 
		Settlement.Date = as.Date( settlement.dates ), Settlement.Rate = settlement.rates, 
 	        Long.Currency = from.ccy, Short.Currency = to.ccy );

  return( output );
};

###########################################################################################
# Routine: is.equal.option.symbols
#
# Determine whether two option symbols are equivalent
###########################################################################################

is.equal.option.symbols <- function( symbol1, symbol2 ) {
  parsed1 = parse.option.symbol( symbol1 );
  parsed2 = parse.option.symbol( symbol2 );

  short.sym1 = parsed1$Short.Option.Symbol;
  short.sym2 = parsed2$Short.Option.Symbol;

  # Check to see if the options symbols agree (without matching the as of date )
  if( short.sym1 != short.sym2 ) {

    # If the option symbols don't agree, then they could still be different versions of the same symbol,
    #    viewed at different as-of-dates (separated by some stock split).  Get the split info.
    split.adj.symbols1 = get.split.adj.option.symbol.ts( parsed1$Option.Symbol, parsed1$asOfDate );
    if( nrow( split.adj.symbols1 ) > 0 && short.sym2 %in% split.adj.symbols1$Symbol ) {
    
      # Create a new symbol for option 1, such that symbols 1 and 2 have the same root, but different as-of-dates 
      idx = min( which( split.adj.symbols1$Symbol == short.sym2 ) );
      equal.short.symbol1 = split.adj.symbols1$Symbol[idx]
      new.date = split.adj.symbols1$Date[idx];
      equal.symbol1 = sprintf( '%s_%d', equal.short.symbol1, 1e4 * year(new.date) + 1e2 * month(new.date) + day(new.date) );

      # Now recurse to compare the symbols with the same root but different as-of-dates and see if they are the same
      result = is.equal.option.symbols( equal.symbol1, symbol2 ); 
      return( result );
    } else {
      return( FALSE );
    };  
  } else {
    min.date = min( parsed1$asOfDate, parsed2$asOfDate ); 
    max.date = max( parsed1$asOfDate, parsed2$asOfDate ); 
    split.ts = prod.ts( -1 + get.stock.split.ts( parsed1$Symbol, start.date = min.date - 7 ) );
    splits = ts.range( fill.missing( split.ts ), start.date = min.date, end.date = max.date );
    if( all( as.numeric( splits ) == as.numeric(splits[1]) ) ) {
      return( TRUE );
    } else { 
      return( FALSE );
    }; 
  };
};

###########################################################################################
# Routine: match.option.symbols
#
# Match across two arrays of option symbols
###########################################################################################

match.option.symbols <- function( option.symbols1, option.symbols2 ) {
  
  output = rep( NA, length( option.symbols1 ) );
  for( i in 1:length(option.symbols1) ) {

    j = 1;
    while( is.na(output[i]) && j <= length(option.symbols2) ) {
      res = is.equal.option.symbols( option.symbols1[i], option.symbols2[j] );
      if( res ) {
        output[i] = j;
      } else {
        j = j+1;
      };
    };
  };

  return( output );
};


###########################################################################################
# Routine: is.option.symbol
#
# Determine whether a symbol obeys the rules for a consistent option symbol
###########################################################################################

is.option.symbol <- function( symbols ) {
 
  output = logical(length(symbols)); 
  for( i in 1:length(symbols) ) {
    symbol = symbols[i];

    if( is.fx.forward.symbol( symbol ) ) {
      output[i] = FALSE;
    } else {
      info = parse.option.symbol( symbol );
      output[i] =!identical( NA, info );
    };
  };

  return( output );
};

###########################################################################################
# Routine: parse.option.symbols
#
# Extract Symbol, Strike, Expiration, and Type info from option symbol
###########################################################################################

parse.option.symbols <- function( Option.Symbols ) {
  parsed = list( length( Option.Symbols ) );

  for( k in 1:length( Option.Symbols ) ) {
    Option.Symbol = Option.Symbols[k];
    parsed[[k]] = parse.option.symbol( Option.Symbol ); 
  }

  df = c();
  for( n in names( parsed[[1]] ) ) {
    df = cbind( df, unlist( lapply( parsed, function(x) x[n] ) ) );
  };

  df = as.data.frame( df, stringsAsFactors = FALSE );
  colnames( df ) = names( parsed[[1]] );
  rownames( df ) = NULL;

  df$Strike = as.numeric( df$Strike );
  df$Expiration.Date = as.Date( as.numeric( df$Expiration.Date ) );
  df$asOfDate = as.Date( as.numeric( df$asOfDate ) );
  
  return( df );
};

###########################################################################################
# Routine: parse.option.symbol
#
# Extract Symbol, Strike, Expiration, Type and asOfDate info from option symbol
###########################################################################################

parse.option.symbol <- function( option.symbol ) {

  # First, try to get the as-of-date if it is present
  symbol.and.asOfDate = strsplit( option.symbol, '_' );
  if( length( symbol.and.asOfDate[[1]] ) == 2 ) {
    opt.symbol = symbol.and.asOfDate[[1]][[1]];

    # The short symbol is the option symbol without as-of-date information
    short.symbol = opt.symbol;
    
    # Parse the as-of-date
    asOfDate = as.Date.yyyymmdd( symbol.and.asOfDate[[1]][[2]] );
  } else {
    opt.symbol = option.symbol;
    short.symbol = option.symbol;
    asOfDate = NA;
  };

  # Split the symbol into individual characters 
  chars  = strsplit( opt.symbol, '' )[[1]];  
  len = length( chars ); 

  # A proper option symbol (without as-of-date) should be at least 16 characters long
  if( len <  16 ) {
    return( NA );
  };

  # Get Strike information
  Strike = as.numeric( paste( chars[ (len-7):len ], collapse = '' ) ) / 1000;
  Type   = chars[ len - 8 ];
  
  # Find Expiration Date
  Year2 = paste( chars[ (len-14):(len-13) ], collapse = '' );
  Month = paste( chars[ (len-12):(len-11) ], collapse = '' );
  Day   = paste( chars[ (len-10):(len- 9) ], collapse = '' );

  Expiration.Date = as.Date( sprintf( '20%02d-%s-%s', as.numeric( Year2 ), Month, Day ) );

  # If there is no as-of-date, then use the expiration date as the as-of-date
  if( is.na( asOfDate ) ) {
    asOfDate = as.Date.yyyymmdd( Expiration.Date );
    asOfDate.string = gsub( '-', '', as.character( as.Date.yyyymmdd( today() ) ) );
    option.symbol = paste( c( short.symbol, asOfDate.string ), collapse = '_' );
  };

  # Get the symbol of the underlier
  Symbol = paste( chars[ 1:(len-15) ], collapse = '' );

  # Put all of the information into a list
  option.info = list( Symbol = Symbol, Type = Type, Strike = Strike, Expiration.Date = Expiration.Date, 
		Option.Symbol = option.symbol, Short.Option.Symbol = short.symbol, asOfDate = asOfDate  );

  return( option.info );
};

###########################################################################################
# Routine: is.derivative
#
# Determine whether a symbol is an Option or FX Forward symbol
###########################################################################################

is.derivative.symbol <- function( symbols ) {
  result = is.option.symbol( symbols ) | is.fx.forward.symbol( symbols );
  return(result); 
};

###########################################################################################
# Routine: get.option.underliers
#
# Extract Underlier info from option symbol
###########################################################################################

get.option.underlier <- function( Option.Symbols ) {
  parsed = parse.option.symbols( Option.Symbols );
  underliers = parsed$Symbol;
  return( underliers );
}

###########################################################################################
# Routine: get.fx.forward.underlier
#
# Extract Underlier info from FX Forward symbol
###########################################################################################

get.fx.forward.underlier <- function( fx.fwd.symbols ) {
  parsed = parse.fx.forward.symbols( fx.fwd.symbols );
  underliers = parsed$Currency.Cross;
  return( underliers );
}

###########################################################################################
# Routine: get.derivative.underlier
#
# Extract Underlier info from Options and FX Forward symbol
###########################################################################################

get.derivative.underlier <- function( symbols ) {
  underliers = character( length(symbols) );

  opt.idx = is.option.symbol( symbols );
  fwd.idx = is.fx.forward.symbol( symbols );

  underliers[ opt.idx ] = get.option.underlier( symbols[ opt.idx ] );
  underliers[ fwd.idx ] = get.fx.forward.underlier( symbols[ fwd.idx ] );
  return( underliers );
};

###########################################################################################
# Routine: get.split.adjustment
#
# Get the adjustment amount for the quantity of an asset between the start/end dates
###########################################################################################

get.split.adjustment <- function(
  symbol, 
  start.date, 
  end.date = last.biz.date( today() )
) {
  if( is.equity.symbol( symbol ) ) {
    stock.splits = get.stock.split.ts( symbol, start.date, end.date );
    if( is.empty( stock.splits ) ) {
      return(1);
    } else {
      return( prod( as.numeric( stock.splits ) ) );
    };
  } else if( is.option.symbol( symbol ) ) {
    info = parse.option.symbol( symbol );
    adj.amt = get.split.adjustment( info$Symbol, start.date, end.date );
    return( adj.amt );
  } else {
    return(1);
  };
};

###########################################################################################
# Routine: get.split.adj.option.symbol.ts
#
# Get the option symbols at any given point, adjusting for splits
###########################################################################################

get.split.adj.option.symbol.ts <- function( 
  Option.Symbol, 
  asOfDate, 
  start.date = today() %m-% years(10),
  end.date = last.biz.date( today() )
) {
  info = parse.option.symbol( Option.Symbol );
  stock.split.ts = get.stock.split.ts( info$Symbol, start.date = start.date, end.date = end.date );
  if( !( as.Date( asOfDate ) %in% as.Date( row.names( stock.split.ts ) ) ) ) {
    idx = which( as.Date(asOfDate ) >= as.Date( row.names( stock.split.ts ) ) );
    if( length(idx) == 0 ) {
      idx = 1;
    } else {
      idx = min(idx);
    };

    stock.split.ts = merge( stock.split.ts, timeSeries( 1, asOfDate, colnames( stock.split.ts ) ) );
  };

  cum.ts =  timeSeries( cumprod( stock.split.ts ), rownames( stock.split.ts ) )
  idx = which( asOfDate >= as.Date( row.names( cum.ts ) ) );
  if( any(idx) ) {
    cum.ts = cum.ts / as.numeric( cum.ts[ max(idx) ] );
  };

  uniq.dates = as.Date( row.names( stock.split.ts ) );
  split.symbols = data.frame( Date = uniq.dates, Symbol = rep( '', length( uniq.dates ) ), Quantity = as.numeric( cum.ts ) );
  for( k in 1:length( uniq.dates ) ) {
    adj.strike = info$Strike * as.numeric( cum.ts[ uniq.dates[k] ] );
    split.symbols[k,'Symbol'] = create.option.symbol( Symbol = info$Symbol, Strike = adj.strike, 
						Type = info$Type, Expiration.Date = info$Expiration.Date );
  };
   
  return( split.symbols );
};

###########################################################################################
# Routine: create.option.symbol
#
# Construct a standard option symbol from Symbol, Strike, Type, and Expiration info
###########################################################################################

create.option.symbol <- function( 
  Symbol, 
  Strike, 
  Type, 
  Expiration.Date, 
  asOfDate = NA
) {
  option.symbol = sprintf( '%s%d%02d%02d%s%08d', Symbol, year( Expiration.Date ) %% 1000, month( Expiration.Date ), 
						day( Expiration.Date ), Type, round( Strike * 1000 ) );

  if( !is.na( asOfDate ) ) {
    option.symbol = paste( option.symbol, format( asOfDate, '%Y%m%d' ), sep = '_' );    
  };

  return( option.symbol );
};

###########################################################################################
# Routine: get.underliers
# 
# For equities, just return the equity symbol.  For options and forwards, return the underlier symbol
###########################################################################################

get.underliers <- function( symbols ) {
  ac = get.asset.class( symbols );

  # Start with just the symbols
  underliers = symbols;
  
  # Replace option symbols with their underliers
  option.idx = ( ac == 'Option' )
  if( any( option.idx ) ) {
    underliers[ option.idx ] = get.option.underlier( symbols[ option.idx ] )
  };

  # Replace fx forward symbols with their underliers
  fx.fwd.idx = ( ac == 'CurrencyForward' )
  if( any( fx.fwd.idx ) ) {
    underliers[ fx.fwd.idx ] = get.fx.forward.underlier( symbols[ fx.fwd.idx ] )
  };

  return( underliers );
};

###########################################################################################
# Routine: get.denominated.currency
# 
# Get the denomination currency of the asset ( or its underlier )
###########################################################################################

get.denominated.currency <- function( symbols ) {

  # Get the underliers for options and fx forwards
  underliers = get.underliers( symbols );

  # Initialize the output array
  denominated = rep( '', length( underliers ) );

  # Get the asset classes of the underliers
  ac = get.asset.class( underliers );

  # For cash symbols, take the prefix
  if( any( ac == 'Cash' ) ) {
    cash.idx = ( ac == 'Cash' );
    denominated[ cash.idx ] = substr( underliers[ cash.idx ], 1, 3 );
  };

  # For equities, first check in the asset classification file
  if( any( ac == 'Equity' ) ) {
    # Find the location and symbols for equities
    eq.locs = which( ac == 'Equity' );
    eq.symbols = underliers[ eq.locs ];

    # First check the asset classification csv file for denominated ccy info
    eq.info = get.asset.classification( eq.symbols );
    denominated[ eq.locs ] = eq.info$Denominated.Currency;

    # If any equity denominated info is still missing, then check the stock exchange info
    missing.locs = is.na( denominated[ eq.locs ] );
    if( any( missing.locs ) ) {
      # look up the exchange, and then use the lookup table for that exchange
      missing.eq.symbols = eq.symbols[ missing.locs ];
      equity.info = parse.equity.symbols( missing.eq.symbols );
      denominated[ missing.locs ] = equity.info$Currency;
    };
  };

  # For currency pairs (and FX forwards), get the long currency
  if( any( ac == 'CurrencyPair' ) ) {
    ccy.pair.info = parse.currency.pair.symbols( underliers[ ac == 'CurrencyPair' ] );
    denominated[ ac == 'CurrencyPair' ] = ccy.pair.info$Short.Currency;
  };

  return( denominated );
};

###########################################################################################
# Routine: get.exposure.currency
# 
# Get the exposure currency of the asset ( or its underlier )
###########################################################################################

get.exposure.currency <- function( symbols ) {

  # Get the underliers for options and fx forwards
  underliers = get.underliers( symbols );

  # Initialize the output array
  exposure.ccy = rep( '', length( underliers ) );

  # Get the asset classes of the underliers
  ac = get.asset.class( underliers );

  # For cash symbols, take the prefix
  if( any( ac == 'Cash' ) ) {
    cash.idx = ( ac == 'Cash' );
    exposure.ccy[ cash.idx ] = substr( underliers[ cash.idx ], 1, 3 );
  };

  # For equities, first check in the asset classification file
  if( any( ac == 'Equity' ) ) {

    # Find the location and symbols for equities
    eq.locs = which( ac == 'Equity' );
    eq.symbols = underliers[ eq.locs ];

    # First check the asset classification csv file for exposure ccy info
    eq.info = get.asset.classification( eq.symbols );
    exposure.ccy[ eq.locs ] = eq.info$Exposure.Currency;
  };

  # For currency pairs (and FX forwards), get the long currency
  ccy.idx = which( ac == 'CurrencyPair' );
  if( length( ccy.idx ) ) {
    for( c in ccy.idx ) {
      ccy.pair.info = parse.currency.pair.symbols( underliers[c] );
      if( ccy.pair.info$Short.Currency == 'USD' ) {
        exposure.ccy[c] = ccy.pair.info$Long.Currency;
      } else if( ccy.pair.info$Long.Currency == 'USD' ) {
        exposure.ccy[c] = ccy.pair.info$Short.Currency;
      } else {
        stop( 'Getting the denom ccy is unsupported for FX forwards that do not have either leg in USD.' );
      };
    };
  };

  return( exposure.ccy );
};


###########################################################################################
# Routine: get.etf.info
# 
# Return information for all ETFs/Indexes in the .csv file
###########################################################################################

get.etf.info <- function( Symbols = NULL ) {
  all.etf.info <- read.csv( ETF_INFORMATION_CSV, sep=",", header=TRUE, stringsAsFactors = FALSE );

  if( is.empty( Symbols ) ) {
    output = all.etf.info;
  } else {
    asset.row.locs <- match( Symbols, all.etf.info$Symbol );

    if( length( asset.row.locs ) == 0 ) {
      output = data.frame();
    } else {
      output =  all.etf.info[ asset.row.locs, ];
    };
  };

  return( output );
};


###########################################################################################
# Routine: get.etf.info
# 
# Return information for all ETFs/Indexes in the .csv file
###########################################################################################

get.asset.classification <- function( symbols = NA ) {

  asset.classification <- read.csv( ASSET_CLASSIFICATION_CSV, sep=",", header=TRUE, stringsAsFactors = FALSE );

  if( is.null( symbols ) ) {
    ac = asset.classification[ NULL, ];
  } else if( length(symbols) == 1 && is.na(symbols) ) {
    ac = asset.classification;
  } else {
    asset.class = get.asset.class( symbols );
    underliers = get.underliers( symbols ); 
    
    ac.df = data.frame( Symbol = symbols, Underlier = underliers, AssetClass = asset.class );

    idx = match( underliers, asset.classification$Symbol );
    nan_idx = is.na(idx); 
    idx[ nan_idx ] = 1;
    ac = asset.classification[ idx, ];

    if( any( nan_idx ) ) {
      for( col in colnames( asset.classification ) ) {
        if( col == 'Symbol' ) {
          ac[ nan_idx, col ] = symbols[ nan_idx ]; 
        } else if( col == 'Denominated.Currency' ) {
          ac[ nan_idx, col ] = get.denominated.currency( symbols[ nan_idx ] );
        } else if( col == 'Exposure.Currency' ) {
          ac[ nan_idx, col ] = get.exposure.currency( symbols[ nan_idx ] );
        } else {
          ac[ nan_idx, col ] = asset.class[ nan_idx ];
        };
      };
    } else {};

    ac = cbind( ac.df, ac[, setdiff( colnames(ac), 'Symbol' ) ] );
  };

  return( ac );
};



