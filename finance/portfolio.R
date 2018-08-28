

###########################################################################################
# Routine: portfolio.returns
#
# Wrapper for 'run.strategy' function, which rebalances periodically
###########################################################################################

portfolio.returns <- function( 
  levels,
  weights,
  start.date = first.date( levels ), 
  end.date   = last.date( levels ), 
  rebalance.period = NaN
)
{
  alloc.rule = standard.allocation.rules;
  return( run.strategy( levels, weights, start.date = start.date, end.date = end.date, 
				input.args = list( rebalance.period = rebalance.period ), allocation.rule = alloc.rule ) );
};

###########################################################################################
# Routine: standard.allocation.rules
###########################################################################################

standard.allocation.rules <- function( 
  date,
  pf.info, 
  input.args = list( rebalance.period = 30 )
)
{
  i = which( date == pf.info$dates );
  
  pf.info$mktval[ i, ] = ( 1 + pf.info$returns[i, ] ) * pf.info$mktval[ i-1, ];

  rebal = input.args$rebalance.period;
  if( ( class( pf.info$weights ) == 'timeSeries' && as.Date( date ) %in% as.Date( rownames( pf.info$weights ) ) ) ||
      ( !is.null( rebal ) && !is.na( rebal ) &&  date >= pf.info$last.rebalance.date + rebal ) 
  ) {
    pf.info <- standard.rebalancing( pf.info, i );
  };

  return( pf.info );  
};

###########################################################################################
# Routine: max.deviation.alloc.rules
###########################################################################################

max.deviation.alloc.rules  <- function( 
  date, 
  pf.info, 
  input.args = list( rebalance.period = 30, rebalance.threshold = .10, post.rebalance.deviation = 0 )
)
{
  pf.info = standard.allocation.rules( date, pf.info, input.args );

  i = which( pf.info$dates == date );
  deviation <- sum( abs( pf.info$mktval[i, ] / sum( pf.info$mktval[i, ] ) - weights ) );
  
  if( deviation > input.args$rebalance.threshold ) {
    if( 0 < sum( which( 'post.rebalance.deviation' == names( input.args ) ) ) ) {
      alpha  = input.args$post.rebalance.deviation / deviation;
      pf.info$mktval[ i, ] = alpha * pf.info$mktval[i, ] + ( 1 - alpha ) * pf.info$weights * sum( pf.info$mktval[ i, ] );
    } else {
      pf.info <- standard.rebalancing( pf.info, i );
    };
  };

  return( pf.info );
};


###########################################################################################
# Routine: standard.rebalancing
#
# Helper function for the portfolio.returns functions.
# This rebalances portfolios for weights that are vectors and time series.  
###########################################################################################

standard.rebalancing <- function( pf.info, i ) {
  dates   = pf.info$dates;
  weights = pf.info$weights; 

  if( class( weights ) == "numeric" ) {
     pf.info$mktval[ i, ] = sum( pf.info$mktval[ i, ] ) * weights;
  } else {
     if( min( rownames( weights ) ) == dates[i] ) {
       weight.index = min( rownames( weights ) );
     } else {
       weight.index = -1 + match( TRUE, ( rownames( weights ) > dates[i] ) );
       if( is.na( weight.index ) && last.date( weights ) <= dates[i] )
          weight.index = last.date( weights );
     };

     pf.info$mktval[ i, ] = sum( pf.info$mktval[ i, ] ) * weights[ weight.index, ];
  };

  pf.info$last.rebalance.date = dates[i];
  return( pf.info );
};


###########################################################################################
# Routine: run.strategy
###########################################################################################

run.strategy <- function( 
  levels, 
  weights, 
  start.date      = first.date( levels ), 
  end.date    	  = last.date( levels ), 
  input.args      = list(),
  allocation.rule = function( date, pf.info, input.args ) pf.info
)
{
  # Initialize the output variable
  pf.mktval = matrix( NaN, ncol = ncol( levels ), nrow = nrow( levels ) );
  if( class( weights ) == "numeric" ) {
    pf.mktval[1,] = matrix( weights, ncol = ncol( levels ) );
    SD = start.date;
  } else {
    if( start.date == min( rownames( weights ) ) ) {
      initial.weight.loc = 1;
      SD = min( as.Date( rownames( weights ) ) );
    } else {
      initial.weight.loc = -1 + min( which(  rownames( weights ) > start.date ) );
      if( initial.weight.loc == 0 ) {
        stop( 'The start.date selected for this analysis is too early, as there are no weights available at this time.' );
      } else {};
  
      SD = as.Date( rownames( weights )[ initial.weight.loc ] );
    };

    pf.mktval[1,] = matrix( as.numeric( weights[ initial.weight.loc, ] ), nrow = 1, ncol = ncol( levels ) );  
  };
 
  levels    = levels[ rownames( levels ) >= SD, ]; 
  dates     = as.Date( rownames( levels ) );
  pf.mktval = pf.mktval[ 1:length( dates ), ];

  returns = matrix( calc.returns( levels, rm.na = FALSE ),  ncol = ncol( levels ) ); 

  pf.info <- list( dates = dates, returns = returns, mktval = pf.mktval, weights = weights, last.rebalance.date = SD );
  for( i in 2:length( dates ) ) {
    pf.info = allocation.rule( dates[i], pf.info, input.args );

    # Make sure that the portfolio value is positive.    
    if( sum( pf.info$mktval[ i-1, ] ) <= 0 )
       pf.info$mktval[ i, ] = pf.info$mktval[ i, ] * 0;
  }; 
 
  pf.mktval = pf.info$mktval;
  total     = rowSums( pf.mktval ); 
  mktval.ts = timeSeries( cbind( pf.mktval, total ), dates, units = c( names( levels ), "Total" ) );

  return( mktval.ts );
};

###########################################################################################
# Routine: portfolio.returns.avg
###########################################################################################

portfolio.returns.avg <- function( 
  levels,
  weights,
  start.date = first.date( levels ), 
  end.date   = last.date( levels ), 
  rebalance.period = 30
)
{
  start.dates = rownames( levels )[ start.date <= rownames( levels ) & rownames( levels ) < start.date + rebalance.period ];

  for( sd in start.dates ) {
    tmp = portfolio.returns( levels, weights, start.date = as.Date(sd), end.date = end.date, rebalance.period = rebalance.period )  / length( start.dates ); 
    missing.dates = setdiff( start.dates, rownames( tmp ) );
    for( md in missing.dates ) {
       tmp = merge( pf[1,], timeSeries( 1 / length( start.dates ), md ) );
    };

    if( sd == start.dates[1] ) {
      pf = tmp;
    } else {
      pf = pf + tmp;
    };
  };  
 
  return( pf );
};

