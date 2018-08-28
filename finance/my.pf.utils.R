
library( 'R2HTML' );
source( paste( FINANCE_PATH, "/_Trade.R", sep = "" ) );

###########################################################################################
# Routine: get.pf.transactions
#  
# Get all transactions from the .csv file, and construct a data.frame from the information.
# If 'add.cash' is TRUE, we add an additional USDCash transaction with the opposite dollar
# value, representing the change in Cash in the portfolio after the transaction.
###########################################################################################

get.pf.transactions <- function( 
  transaction.log = TRANSACTIONS_CSV, 
  trade.name = '', 
  handle.expired.forwards = TRUE
) {
  # Define the column names that will be read in from the transaction data .csv file
  colClasses = c( Transaction.Type = "character", Transaction.Date="character",Quantity="numeric",Symbol="character",
    Description="character", Activity.Class="character", Trade.Price="numeric", Settlement.Amount="numeric", 
			Settlement.Ccy="character", Trade.Name = 'character' );

  # Read the raw data from the .csv file, and order rows by increasing date
  trans <- read.csv( transaction.log, sep=",", header=TRUE, colClasses = colClasses );
  trans$Transaction.Date = as.Date( trans$Transaction.Date, '%m/%d/%y' ); 

  # Restrict to a single trade if one is specified.  Otherwise, take all the transactions
  if( trade.name != '' ) {
    trans = trans[ trans$Trade.Name == trade.name, ];
  };

  # If the transaction is on an option, multiply the trade price by 100 since the option conttract is written on 100 shares.
  option.inds = as.numeric( unlist( lapply( trans$Symbol, function(x) is.option.symbol(x) ) ) );
  option.multiplier  = 1 + 99 * option.inds;

  # Calculate the Transaction Cost
  Transaction.Cost <- abs( trans[ , "Trade.Price" ] * trans[ , "Quantity" ] * option.multiplier - trans[ , "Settlement.Amount" ] ) 

  # Determine whether the trade is a net debit or net credit
  debit.type = c( grep( 'Sold', trans$Activity.Class ), grep( 'Withdrawal', trans$Transaction.Type ) ) 
  Direction <- as.numeric( 1:nrow( trans ) %in% debit.type ) * -2 + 1;

  # Append the 'Direction' information to the original .csv info
  trans <- cbind( trans, Transaction.Cost, Direction );

  # Remove trailing whitespace from character columns
  for( col in names( colClasses ) ) {
    if( colClasses[ col ] == 'character' ) {
      trans[,col] <- gsub(" $","", trans[,col], perl=T)
    };
  };

  # Force the transaction date column to be of type 'date'
  trans$Transaction.Date = as.Date( trans$Transaction.Date );

  # Add rows for FX Forwards that have expired
  fx.fwd.symbols = unique( trans$Symbol[ is.fx.forward.symbol( trans$Symbol) ] );

  for( fx.fwd.symbol in fx.fwd.symbols ) {
    fwd.info = parse.fx.forward.symbols( fx.fwd.symbol );
    if( fwd.info$Settlement.Date > last.biz.date() ) {
      next;
    } else {};

    row = trans[ trans$Symbol == fx.fwd.symbol, ];
    if( nrow( row ) > 1 ) {
      stop( 'Unsupported for more than two transactions with the same FX forward.' );
    } else {};
 
    # Add a row to close out the forward contract 
    close.row = row;
    close.row$Direction = -row$Direction;
    close.row$Transaction.Date = as.Date( fwd.info$Settlement.Date );
    close.row$Activity.Class = ifelse( row$Activity.Class == 'Securities Purchased', 'Securities Sold', 'Securities Purchased' );
    close.row$Transaction.Type = ifelse( row$Transaction.Type == 'Buy', 'Sale', 'Buy' );
    trans = rbind( trans, close.row );

    # Add lines to represent the execution of FX forwards
    if( handle.expired.forwards ) {

      # Add a row to represent the long leg of the currency forward
      if( row$Direction == 1 ) {
        trans.type = 'Buy';
        activity.class = 'Securities Purchased';
      } else {
        trans.type = 'Sell';
        activity.class = 'Securities Sold';
      };

      long.short.row = data.frame( Transaction.Type = trans.type, Transaction.Date = as.Date( fwd.info$Settlement.Date ),
                                Quantity = row$Quantity, Symbol = paste( fwd.info$Long.Currency, 'Cash', sep = '' ),
                                Description = fwd.info$Long.Currency, 
				Activity.Class = activity.class,
				Trade.Price = fwd.info$Settlement.Rate, 
				Settlement.Amount = row$Quantity * fwd.info$Settlement.Rate,
				Settlement.Ccy = fwd.info$Short.Currency,
                                Trade.Name = row$Trade.Name, Transaction.Cost = 0, Direction = row$Direction );

      # Add rows to transactions
      trans = rbind( trans, long.short.row );
    };
  }; 

  # Cast transaction dates to date type
  trans$Transaction.Date = as.Date( trans$Transaction.Date );

  # Order by transaction date
  trans = trans[ order( trans$Transaction.Date ), ];

  return( trans );
};

###########################################################################################
# Routine: get.pf.trade.prices
###########################################################################################

get.pf.trade.prices <- function( 
  transaction.log = TRANSACTIONS_CSV,
  trade.name = '',
  include.costs = TRUE, 
  base.ccy = 'USD'
) {
    trans = get.pf.transactions( transaction.log = transaction.log, trade.name = trade.name );
    base.ccy.cash = paste( base.ccy, 'Cash', sep = '' );

    symbols = sort( unique( trans$Symbol ) );
    trade.dates = sort( unique( as.Date( trans$Transaction.Date ) ) );

    prices = matrix( NaN, nrow = length( trade.dates ), ncol = length( symbols ) );

    for( row in 1:length( trade.dates ) ) {
      dt = as.Date( trade.dates[row] );
      sub.trans = trans[ trans$Transaction.Date == dt, ];

      sub.symbols = unique( sub.trans$Symbol );
      for( k in 1:length( sub.symbols ) ) {
        symbol = sub.symbols[k];

        if( symbol == base.ccy.cash ) {
          VWAP = 1;
        } else {
          sym.trans = sub.trans[ sub.trans$Symbol == symbol, ];
          qty = sym.trans$Quantity;
          if( include.costs ) {
            prc = sym.trans$Settlement.Amount / sym.trans$Quantity;
            if( is.option.symbol( symbol ) ) {
              prc = prc / 100; 
            } else {};
          } else {
            prc = sym.trans$Trade.Price;
          };

          VWAP = sum( qty * prc ) / sum( qty );
        };

        col = which( symbol == symbols );
        prices[ row, col ] = VWAP;
      };
    };

    priceTS = timeSeries( prices, as.Date( trade.dates ), units = symbols );
    return( priceTS );
};

###########################################################################################
# Routine: get.pf.quantities
#
# Calculate asset quantities within the portfolio
###########################################################################################

get.pf.quantities <- function(
  symbol = '',
  trade.name = '',
  transaction.log = TRANSACTIONS_CSV, 
  base.ccy = 'USD'
) {
  # Get transactions
  trans <- get.pf.transactions( transaction.log = transaction.log, trade.name = trade.name );

  # Order transactions by Date
  trans <- trans[ order( trans$Transaction.Date ), ];

  # If we are evaluating a specific trade, then filter the transactions on that trade's name
  if( symbol != '' ) {
     trans = trans[ trans$Symbol == symbol, ];
  };

  # If we are evaluating a specific trade, then filter the transactions on that trade's name
  if( trade.name != '' ) {
     trans = trans[ trans$Trade.Name == trade.name, ];
  };

  # Find the set of dates with transaction info, and the list of relevant symbols
  trans.dates = as.Date( trans$Transaction.Date );
  symbols = unique( trans$Symbol );

  # Get the stock split information
  stock.split.ts = get.stock.split.ts( symbols, start.date = min( as.Date( trans$Transaction.Date ) ), end.date = today() );
  stock.split.ts = stock.split.ts[ rowSums( stock.split.ts == 1 ) != ncol( stock.split.ts ), ];  # Remove non-split rows
  if( nrow( stock.split.ts ) > 0 ) {
    split.dates = as.Date( rownames( stock.split.ts ) );
  } else {
    split.dates = NULL;
  };

  # Add funding currency cash symbols to the list of symbols (if they aren't already there)
  funding.ccys.cash = paste( unique( trans$Settlement.Ccy ), 'Cash', sep = '' );
  new.fund.ccy = setdiff( funding.ccys.cash, symbols );
  symbols = c( symbols, new.fund.ccy );
  
  # Find the unique set of transaction dates
  uniq.dates <- sort( unique( c( trans.dates, split.dates ) ) );

  # Initialize the array that will contain Quantity info
  Quantity = timeSeries( matrix( 0, nrow = length( uniq.dates ), ncol = length( symbols ) ), uniq.dates, symbols );

  # Loop through each of the dates on which there were transactions
  for( n in 1:length( uniq.dates ) ) {
    # Get the next date
    row.date = as.Date( uniq.dates[n] );
    
    # Unless we are on the first row, copy the quantities from the previous row
    if( n > 1 ) {
      Quantity[n,] = Quantity[n-1,];
    } else {};

    # Find the set of raws corresponding to transactions on this date
    trans.row.idxs = which( row.date == trans.dates ); 

    # Loop through all the transactions corresponding to this date
    for( trans.row.idx in trans.row.idxs ) {
      row = trans[ trans.row.idx, ];
      mult = row$Direction;

      # Add the new quantity to the existing 
      Quantity[ n, row$Symbol ] <- Quantity[ n, row$Symbol ] + mult * row$Quantity;

      # If the transaction is not a deposit/withdrawal, then remove/add the cash involved in the transaction
      if( !( row$Transaction.Type %in% c( 'Deposit', 'Withdrawal' ) ) ) {
	# If we are evaluating the whole portfolio, then fund the trade from the funding currency
        funding.ccy.cash = paste( row$Settlement.Ccy, 'Cash', sep = '' );
        Quantity[ n, funding.ccy.cash ] <- Quantity[ n, funding.ccy.cash  ] - mult * row$Settlement.Amount;
      }; 
    };

    # Adjust for stock splits
    if( row.date %in% split.dates ) {
       split.row = stock.split.ts[ row.date,  as.numeric( stock.split.ts[ row.date, ] ) != 1 ]
       split.symbols = colnames( split.row );
       for( split.symbol in split.symbols ) {

         split.amt = as.numeric( stock.split.ts[ row.date, split.symbol ] );

         pos.wt.loc = abs( as.numeric( Quantity[ n, ] ) ) > 0;
         split.sym.loc = ( symbols == split.symbol );
         asset.locs = which( pos.wt.loc & split.sym.loc );
    
         if( split.amt == 1 || length( asset.locs ) == 0 ) {
           next;
         };

         for( asset.loc in asset.locs ) {
           symbol = symbols[asset.loc];

           if( !is.option.symbol( symbol ) ) {
             Quantity[n,asset.loc] = as.numeric( Quantity[n,asset.loc] ) / split.amt;
           } else {};
         };
       }; 
    };
  };

  return( Quantity );
}

###########################################################################################
# Routine: get.pf.flows
#
# From the CSV file storing the portfolio transactions, this function constructs a time
# series of deposits and withrawals into the portfolio
###########################################################################################

get.pf.flows <- function(
  transaction.log = TRANSACTIONS_CSV, 
  trade.name = '', 
  start.date = today() %m-% years(10), 
  asOfDate = last.biz.date( today() ), 
  base.ccy = 'USD'
) {
  # Get the transaction list
  trans <- get.pf.transactions( transaction.log = transaction.log, trade.name = trade.name, handle.expired.forwards = TRUE );

  # Get the unique symbols and dates appearing in the flows
  uniq.symbols = unique( trans$Symbol );
  uniq.dates = unique( trans$Transaction.Date );

  # Initialize an empty matrix to store the flow data
  data = matrix( 0, ncol = length(uniq.symbols), nrow = length(uniq.dates) );

  # Loop through each unique symbol and store the flow information
  for( col in 1:length( uniq.symbols ) ) {

    # Get the flow info for this particular symbol
    symbol = uniq.symbols[col];
    sym.flows = trans[ trans$Symbol == symbol, ];

    # Get the direction of the trade
    direction = sym.flows$Direction;

    # Find the funding source currency for each transaction
    funding.source = paste( sym.flows$Settlement.Ccy, 'Cash', sep = '' );

    # Get the flow info for this symbol
    output.rows = match( sym.flows$Transaction.Date, uniq.dates );
    flow.amounts.settlement.ccy = sym.flows$Settlement.Amount * direction;

    # Loop through the info for each transaction for this symbol
    for( k in 1:length(output.rows) ) {
      row = output.rows[k];
      amt.settle.ccy = flow.amounts.settlement.ccy[k];
      settle.ccy = sym.flows$Settlement.Ccy[k];
      settle.date = as.Date( sym.flows$Transaction.Date[k] );

      # If the settlement currency isn't the same as the base currency, then convert using end-of-day prices
      if( settle.ccy != base.ccy ) {
        amt = convert.currency( amt.settle.ccy, from.ccy = settle.ccy, to.ccy = base.ccy, asOfDate = settle.date );
      } else {
        amt = amt.settle.ccy;
      };

      data[ row, col ] = data[ row, col ] + amt;

      # If the flow is not a cash withdrawal/deposit, then fund the flow with cash
      trans.type = sym.flows$Transaction.Type[k];
      if( !( trans.type %in% c( 'Deposit', 'Withdrawal' ) ) ) {
        funding.source = paste( settle.ccy, 'Cash', sep = '' );
        if( length( unique( funding.source ) ) > 1 ) {
          stop( 'Getting flows is not supported when one symbol has multiple funding sources.' );
        };
        funding.col = which( funding.source[1] == uniq.symbols );
        data[ row, funding.col ] = data[ row, funding.col ] - amt;
      };
    };
  };

  # Create a time series with the flow info
  flows.ts = timeSeries( data, uniq.dates, units = uniq.symbols );

  # Restrict flows to be between start/end dates
  flows.ts = ts.range( flows.ts, start.date = start.date, end.date = asOfDate );

  return( flows.ts );
};
 

##########################################################################################
# Routine: construct.mktval
#
# Given quantities and prices, construct the market value
###########################################################################################

construct.mktval <- function(
  Qty,
  prices,
  asOfDate = last.biz.date( today() )
) {
  start.date <- as.Date( min( rownames( Qty ) ) );

  # Remove price columns that do not appear in 'Qty'
  prices <- prices[ , colnames( Qty ) ];

  # Make sure price information is available on all dates that Qty is defined
  all.Qty   <- fill.missing( Qty, end.date = asOfDate, only.weekdays = TRUE );
  all.dates <- rownames( prices )[ rownames( prices ) >= start.date ];
  all.Qty   <- all.Qty[ all.dates, ];

  # Get MktVal from prices and Qty, and construct a time series
  MktVal <- prices[ all.dates, ] * all.Qty;
  MktVal[ is.na( MktVal ) ] = 0;
  total  <- timeSeries( rowSums( matrix( MktVal, ncol=ncol( MktVal ) ) ), rownames( MktVal ), units="Total" );
  MktVal <- cbind( total, MktVal );
  return( MktVal );
};

###########################################################################################
# Routine: initialize.trade
#
# Get a Trade class object with all transaction information
###########################################################################################

initialize.trade <- function(
  trade.name = '',
  asOfDate = last.biz.date( today() )
) {
  
  trades = new( 'Trade', TradeName = trade.name );
  trades = setMktValTS( trades );
  trades@EndDate = asOfDate;

  return( trades )
};

###########################################################################################
# Routine: get.current.positions
###########################################################################################

get.current.positions <- function( 
  transaction.log = TRANSACTIONS_CSV, 
  trade.name = '', 
  asOfDate = last.biz.date( today() )
) {
  all.qty = get.pf.quantities( transaction.log =  transaction.log, trade.name = trade.name );
  dates = as.Date( row.names( all.qty ) );

  idx = max( which( dates <= asOfDate ) );
  qty = all.qty[ idx, ];
  qty = as.data.frame( qty[ ,qty != 0 ] );
 
  ac.info = get.asset.class.list( row.names( qty ) ); 
  asset.classes = names( ac.info );

  symbols = c();
  quantities = c();
  for( asset.class in asset.classes ) {
    assets = ac.info[[ asset.class ]];
    asset.qty = qty[ assets, ];
    quantities = c( quantities, asset.qty[ order( assets ) ] );
    symbols = c( symbols, sort( assets ) );
  };

  output = data.frame( Symbol = symbols, Quantity = quantities );
  return( output );
};

###########################################################################################
# Routine: get.portfolio
#
# Returns a portfolio object
###########################################################################################

get.portfolio <- function( asOfDate = NULL ) {

  pf_trade = initialize.trade();

  if( is.null( asOfDate ) ) {
    pf = Portfolio( pf_trade );
  } else {
    pf = Portfolio( pf_trade, asOfDate = asOfDate );
  };

  pf = setOptionInfo(pf);

  return(pf);
};

###########################################################################################
# Routine: get.trade.quantities
#
# Get the quantities of underliers in each trade
###########################################################################################

get.trade.quantities <- function( 
  asOfDate = today(), 
  transaction.log = TRANSACTIONS_CSV, 
  trade.name = '' 
) {
  trans = get.pf.transactions( transaction.log = transaction.log, trade.name = trade.name );
  trades = setdiff( unique( trans$Trade.Name), 'USDCash' );

  underliers = unique( trans$Symbol );

  quantities = matrix( 0, ncol = length( underliers ), nrow = length( trades ) );
  colnames( quantities ) = underliers;
  rownames( quantities ) = trades;

  for( i in 1:length(trades) ) {
    trade = trades[i];
    qty = get.pf.quantities( trade.name = trade, transaction.log = transaction.log );
    qty = ts.range( qty, end.date = asOfDate+1 );

    if( is.empty(qty) || last( rowSums( abs( qty ) ) ) == 0 ) {
      next;
    } else {
      quantities[i, match( colnames( qty ), underliers ) ] = as.numeric( tail( qty, 1 ) );
    };
  }; 

  # Remove Underliers which are not used, and trades which have expired
  symbols = colnames(quantities);
  quantities = quantities[ 0 != rowSums(abs(quantities[,!is.cash.symbol(symbols)] ) ), ];
  quantities = quantities[,is.cash.symbol(colnames(quantities)) | colSums(abs(quantities)) > 0 ];

  # Add cash columns as separate trades, and populate the Mkt Val
  cash.symbols = symbols[ is.cash.symbol(symbols) ];
  # Add rows ('trades') for different currencies of cash, if they are not already present
  trade.qty = add.df.rows( quantities, cash.symbols, 0 );
  for( i in 1:length(cash.symbols) ) {
    cash.symbol = cash.symbols[i];
    # Zero out cash entries, which are purely for accounting
    trade.qty[,cash.symbol] = 0;
  }; 

  return( trade.qty );
};


###########################################################################################
# Routine: get.trade.mktval
#
# Get the mktval of underliers in each trade
###########################################################################################

get.trade.mktval <- function( asOfDate = today() ) {
  quantities = get.trade.quantities();
  underliers = colnames( quantities );
  trades = rownames( quantities );

  prices = get.time.series( underliers, start.date = asOfDate - 10, end.date = asOfDate, use.proxy = TRUE );
  last.prices = as.numeric( tail( fill.missing( prices[,underliers ] ), 1 ) );

  # Multiply prices by 100 for options
  last.prices = last.prices * ( 1 + is.option.symbol(underliers) * 99 );

  mktval = quantities %*% diag( last.prices );

  colnames( mktval ) = underliers;
  rownames( mktval ) = trades;

  return( mktval );
};

###########################################################################################
# Routine: get.trade.weights
#
# Get the weights of underliers in each trade
###########################################################################################

get.trade.weights <- function( asOfDate = today() ) {
  mktval = get.trade.mktval( asOfDate );

  trade.mv = rowSums( mktval );
  weights = diag( 1 / as.numeric( abs( trade.mv ) ) ) %*% mktval;

  return( weights );
};

###########################################################################################
# Routine: update.pf.symbol.lists
#
# Find the symbols that are used in the portfolio, and save them to .txt files so that 
#   their market data will be updated.
###########################################################################################

update.pf.symbol.lists <- function( transaction.log = TRANSACTIONS_CSV ) {
  qty = get.pf.quantities( transaction.log = transaction.log );

  # Portfolio symbols are just the column names in the data frame
  symbols = names( qty );

  # Current symbols are the columns with non-zero entries in the last row
  curr.symbols = names( qty[nrow(qty),qty[nrow(qty),]!=0] );

  equity.symbols = symbols[ is.equity.symbol( symbols ) ]; 
  option.symbols = symbols[ is.option.symbol( symbols ) ]; 
  option.underliers = unique( get.option.underlier( option.symbols ) );
  
  curr.equity.symbols = curr.symbols[ is.equity.symbol( curr.symbols ) ];   
  curr.option.symbols = curr.symbols[ is.option.symbol( curr.symbols ) ];   

  if( !is.empty( curr.option.symbols ) ) {
    curr.option.underliers = unique( get.option.underlier( curr.option.symbols ) );
  } else {
    curr.option.underliers = c();
  };

  equity_list =  unique( c( equity.symbols, option.underliers ) );
  option_list = unique( c( curr.equity.symbols, curr.option.underliers ) );
  intraday_equity_list = unique( c( curr.equity.symbols, curr.option.underliers ) );
  intraday_option_list = curr.option.underliers;

  write.table( equity_list, file = EQTY_LIST_CALCULATED , row.names = F, col.names = F, quote = F );
  write.table( option_list, file = OPT_LIST_CALCULATED , row.names = F, col.names = F, quote = F );
  write.table( intraday_equity_list, file = INTRADAY_EQTY_LIST_CALCULATED , row.names = F, col.names = F, quote = F );
  write.table( intraday_option_list, file = INTRADAY_OPT_LIST_CALCULATED , row.names = F, col.names = F, quote = F );
};

###########################################################################################
# Routine: get.pf.equity.list
###########################################################################################

get.pf.equity.list <- function() {
  eod.list = read.table( file = EQTY_LIST_CALCULATED, stringsAsFactors = F  )[,1];
  intraday.list = read.table( file = INTRADAY_EQTY_LIST_CALCULATED, stringsAsFactors = F  )[,1];
  equity.list = unique( c( eod.list, intraday.list ) );
  return(equity.list);
}

###########################################################################################
# Routine: get.pf.option.list
###########################################################################################

get.pf.option.list <- function() {
  eod.list = read.table( file = OPT_LIST_CALCULATED, stringsAsFactors = F  )[,1];
  intraday.list = read.table( file = INTRADAY_OPT_LIST_CALCULATED, stringsAsFactors = F  )[,1];
  option.list = unique( c( eod.list, intraday.list ) );
  return(option.list);
}

###########################################################################################
# Routine: get.pf.capital.gains
#
# Get Cost Basis information for trades
###########################################################################################

get.pf.capital.gains <- function(
  asOfDate = last.biz.date( today() ), 
  tol = 1e-6, 
  transaction.log = TRANSACTIONS_CSV,
  trade.name = ''
) {

  # Get all tradeso so that we can find transaction and Symbol information
  trades = get.pf.quantities( trade.name = trade.name, transaction.log = transaction.log );
  trade.prices = get.pf.trade.prices( trade.name = trade.name, transaction.log = transaction.log );
  raw.PR = as.matrix( trade.prices );

  # Get information about the quantities of individual asset clasess  
  quantities = get.pf.quantities( transaction.log = transaction.log, trade.name = trade.name );
  symbols = setdiff( colnames( quantities ), 'USDCash' );
  QT = as.matrix( quantities );
  all.dates = as.Date( row.names( quantities ) );

  # Add any missing dates to PR (on stock split dates)
  pr.data = matrix( NA, ncol = length(colnames(raw.PR)), nrow = length(all.dates) );
  pr.data[ as.Date( all.dates ) %in% as.Date( row.names( raw.PR ) ), ] = as.matrix( raw.PR );
  pr.data[ is.na( pr.data ) ] = NaN;
  PR = timeSeries( pr.data, all.dates, colnames(raw.PR) );

  # Define the date that will denote a NULL date value in the code below
  NA.date = as.Date( '3000-12-31' );

  # Initialize the tax info data frame
  cg.info = c();

  # Loop through by individual symbols
  for( symbol in symbols ) {

    split.ts = get.stock.split.ts( symbol, start.date = min(all.dates), end.date = max(all.dates) );
    colnames( split.ts ) = 'Split';
    quantity.ts = quantities[,symbol];
    quantity.ts = merge( quantity.ts, split.ts );
    quantity.ts[ is.na( quantity.ts$Split ), 'Split' ] = 1;
    quantity.ts$Split = cumprod( quantity.ts$Split );    
    qty = as.matrix( quantity.ts ); 

    asOfDate.rows = which( as.Date( row.names( quantity.ts ) ) <= last.biz.date( asOfDate ) );
    if( length( asOfDate.rows ) ) {
      asOfDate.split = as.numeric( quantity.ts[ max( asOfDate.rows ), 'Split' ] );
    } else {
      stop( 'AsOfDate is earlier than available data' ); 
    };
 
    # Find the dates where trades occured (adjusting for stock splits)
    trade.idx = c( qty[1,symbol] != 0, qty[-1,symbol] * qty[-1,'Split'] != qty[-nrow(qty),symbol] * qty[-nrow(qty),'Split' ] );
    trade.dates = as.Date( all.dates[ trade.idx ] );

    # Ge the quantity and price of the asset on the trade dates
    quantity = as.numeric( quantity.ts[ trade.idx, symbol ] );
    cum.splits = as.numeric( quantity.ts[ trade.idx, 'Split' ] );
    if( symbol %in% colnames( PR ) ) {
      prices = PR[trade.idx,symbol];
    } else { 
      prices = rep( NaN, length(trade.dates ) );
    };

    # Create a new row with tax info for the first trade date in 'trade.dates'
    new.info = data.frame( Symbol = symbol, Quantity = quantity[1], Start.Date = trade.dates[1], End.Date = NA.date,
                     Split.Adjustment = asOfDate.split / first(cum.splits), Open.Price = prices[1], Close.Price = NaN );
 
    # If there is more than one trade for this symbol, then loop through the rest of the trades as well
    if( length(quantity) > 1 ) {

      for( i in 2:length(quantity) ) {

        # Trade amount is how much the position changed 
        split.adj = ( cum.splits[i] / cum.splits[i-1] );
        amt = quantity[i] - quantity[i-1] / split.adj;

        # Check if this is a new position, or if it is adding to an existing position.  In this case, add a new row
        if( quantity[i-1] == 0 || sign( amt ) == sign( quantity[i-1] ) ) {

          new.info = rbind( new.info, data.frame( Symbol = symbol, Quantity = amt, Start.Date = trade.dates[i],
                      Split.Adjustment = asOfDate.split / cum.splits[i], End.Date = NA.date, 
			Open.Price = prices[i], Close.Price = NaN ) );
        } else {
          # Otherwise, we are taking away from an existing position, so we need to figure out how to adjust the tax row

          tol = 1e-6;
          while( abs(amt) > tol ) {
            # Keep looping through old trade lots until the current trade is completely accounted for

            # Find the row of the oldest trade lot that is still open
            tax.lot.rows = which( new.info$End.Date == NA.date );

            if( !length( tax.lot.rows ) ) {
              # If there are no trade lots still available to close out, then add a new row for the remaining quantity
              new.info = rbind( new.info, data.frame( Symbol = symbol, Quantity = amt, Start.Date = trade.dates[i],
                       Split.Adjustment = asOfDate.split / cum.splits[i], End.Date = NA.date, 
			Open.Price = prices[i], Close.Price = NaN ) );
              amt = 0;
            } else {
              # If there are still trade lots to close out, get the quantity of those trade lots, adjusted for stock splits

              next.row = min( tax.lot.rows );
              next.date.idx = which( new.info[ next.row, 'Start.Date' ] == trade.dates );
              orig.amt = new.info$Quantity[next.row];
              row.split.adj = cum.splits[i] / cum.splits[ next.date.idx ];
              orig.adj.amt = orig.amt / row.split.adj;

              if( abs( orig.adj.amt ) <= abs( amt ) ) {
                new.info[next.row,'End.Date'] = as.Date( trade.dates[i] );
                new.info[next.row,'Close.Price'] = prices[i];
                new.info[next.row,'Split.Adjustment'] = row.split.adj;
                amt = amt + orig.adj.amt;
              } else {
                # Add a row at the end for the shares sold in this transaction
                new.info = rbind( new.info, new.info[next.row,] );
                new.info[ nrow(new.info),'Quantity'] = -amt * row.split.adj;
                new.info[ nrow(new.info),'End.Date'] = as.Date( trade.dates[i] );
                new.info[ nrow(new.info),'Close.Price'] = prices[i];
                new.info[ nrow(new.info),'Split.Adjustment'] = row.split.adj;

                # Change the current row so that the quantity reflects the shares sold in this transaction
                new.info[ next.row,'Quantity'] = orig.amt + amt * row.split.adj;
                amt = 0;
              };
            };
          };
        };
    
        new.info = new.info[ order( as.Date( new.info$Start.Date ) ), ];
        new.info = new.info[ abs( new.info$Quantity ) > tol, ];
      };
    };

    if( is.null( cg.info ) ) {
      cg.info = new.info;
    } else {
      cg.info = rbind( cg.info, new.info );
    };
  };

  # Check that the open positions are consistent with the portfolio quantities 
  check.capital.gains( cg.info, asOfDate = asOfDate, tol = tol );

  # Get rid of row names
  rownames( cg.info ) = NULL;

  # Order rows by the end date
  cg.info = cg.info[ order( cg.info$End.Date ), ];

  # Add column for the number of days outstanding  
  cg.info$Days.Open = NaN;
  unrealized.idx = cg.info$End.Date == NA.date;
  realized.idx = !unrealized.idx;
  cg.info$Days.Open[unrealized.idx] = as.numeric( asOfDate ) - 
						as.numeric( cg.info$Start.Date[unrealized.idx] );
  cg.info$Days.Open[realized.idx] = as.numeric( cg.info$End.Date[realized.idx] ) - 
						as.numeric( cg.info$Start.Date[realized.idx] );

  # Add column for type of capital gains
  cg.info$Type = '';
  st.idx = cg.info$Days.Open < 365;
  cg.info$Type[  st.idx & realized.idx ] = 'Short-Term';
  cg.info$Type[ !st.idx & realized.idx ] = 'Long-Term';
  cg.info$Type[ unrealized.idx ] = 'Unrealized';

  # Add columns for total amounts at open/close of trade
  opt.mult = rep( 1, nrow( cg.info ) );
  opt.mult[ is.option.symbol( cg.info$Symbol ) ] = 100;  # Multiply option quantities by 100
  cg.info$Total.Open.Amount = cg.info$Open.Price * cg.info$Quantity * opt.mult;
  cg.info$Total.Close.Amount = cg.info$Close.Price * cg.info$Quantity / cg.info$Split.Adjustment * opt.mult; 

  # Add column for net proceeds (capital gains)
  cg.info = cbind( cg.info, data.frame( Capital.Gains = rep( NaN, nrow( cg.info ) ) ) );
  pos.idx = cg.info$Quantity > 0;
  cg.info$Capital.Gains[pos.idx] = cg.info$Total.Close.Amount[pos.idx] - cg.info$Total.Open.Amount[pos.idx];
  cg.info$Capital.Gains[!pos.idx] = cg.info$Total.Close.Amount[!pos.idx] - cg.info$Total.Open.Amount[!pos.idx];

  # Add a description of the assets
  trans = get.pf.transactions( transaction.log = transaction.log, trade.name = trade.name );
  cg.info$Description = trans[ match( cg.info$Symbol, trans$Symbol ), 'Description' ];

  # Re-order the columns
  cg.info = cg.info[, c( 'Symbol', 'Start.Date', 'End.Date', 'Quantity', 'Description', 'Split.Adjustment', 'Open.Price', 'Close.Price', 
				'Total.Open.Amount', 'Total.Close.Amount', 'Capital.Gains', 'Type', 'Days.Open' ) ];
  return( cg.info );
};

###########################################################################################
# Routine: check.capital.gains
#
# Run a check to make sure the capital gains calculations are consistent with
#  the portfolio quantities
###########################################################################################

check.capital.gains <- function( 
  cap.gains.info, 
  asOfDate = last.biz.date( today() ), 
  tol = 1e-6
) {

  open.position.info = cap.gains.info[ is.nan( cap.gains.info$Close.Price ), ];
 
  open.qty = open.position.info$Quantity / open.position.info$Split.Adjustment;
  open.df = data.frame( Symbol = open.position.info$Symbol, Quantity = open.qty );

  agg.df = aggregate( Quantity ~ Symbol, open.df, sum );
  agg.df$Quantity = round( agg.df$Quantity * 1e4 ) / 1e4;

  current.positions = get.current.positions();
  current.positions = current.positions[ current.positions$Symbol != 'USDCash', ];

  agg.df = agg.df[ order( agg.df$Symbol ), ];
  cap.df = current.positions[ order( current.positions$Symbol ), ];

  if( !all.equal( cap.df$Symbol, agg.df$Symbol ) ) {
    stop( 'Missing symbols from capital gains info.' );
  } else {};

  if( !all( abs( cap.df$Quantity - agg.df$Quantity ) < tol ) ) {
    stop( 'The quantities obtained in the capital gains calculation are inconsistent.' );
  };
};

###########################################################################################
# Routine: save.cap.gains.info
#
# Save the capital gains info to Excel
###########################################################################################

save.cap.gains.info <- function( 
  asOfDate = last.biz.date( today() ), 
  tol = 1e-6
) {
  cap.gains.info = get.pf.capital.gains( asOfDate = asOfDate, tol = tol );

  filename = sprintf( "%s/calculated/tax_info/capital_gains.csv", DATA_PATH );
  write.table( cap.gains.info, file = filename, row.names = F, col.names = T, quote = F, sep = ',' )
};

###########################################################################################
# Routine: save.pf.ccy.info
#
# Save the exchange rate info needed to convert capital gains into different currencies
###########################################################################################

save.pf.ccy.info <- function(
  ccy.pairs = c( 'GBP/USD', 'EUR/USD', 'CHF/USD' ), 
  start.date = as.Date( '2009-12-31' )
) {
  ts = get.time.series( ccy.pairs, start.date = start.date );

  filename = sprintf( "%s/calculated/tax_info/currency_pairs.csv", DATA_PATH );
  write.table( ts, file = filename, row.names = T, col.names = T, quote = F, sep = ',' )
};

###########################################################################################
# Routine: fill.missing.pf.quantities
#
# Save the exchange rate info needed to convert capital gains into different currencies
###########################################################################################

fill.missing.pf.quantities <- function( 
  orig.qty.ts, 
  fill.dates
) {

  # Fill dates before first trade with 0  
  prior.dates = fill.dates[ fill.dates < min( rownames( orig.qty.ts ) ) ];
  if( length( prior.dates ) > 0 ) {
    qty.ts = na.locf( insert.dates.ts( orig.qty.ts, prior.dates, 0 ) );
  } else {
    qty.ts = orig.qty.ts;
  };

  # Fill all other dates with NA, and then...
  qty.ts = insert.dates.ts( qty.ts, fill.dates, NA );

  # ...replace all NA's with the prior value
  qty.ts = na.locf( qty.ts );

  return( qty.ts );
};


