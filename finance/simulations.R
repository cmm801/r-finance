library( tseries, quietly = TRUE );

###########################################################################################
# Routine: simulate.returns
#
# Simulate a set of returns (or calculate historical returns) given an initial time series
# of levels
###########################################################################################

simulate.returns <- function(
  levels = c(),
  ndays  = NA,
  method = 'Markov',
  symbol = '',
  initial.state = -1,
  block.size = MARKOV_BLOCK_SIZE,
  num.sims   = 1e4
) {
  if( method == 'Historical' ) {
    sim.rtns = c();

    for( n in ndays ) {
      sim.rtns = rbind( sim.rtns, t( ri( levels, n = n, rm.na = FALSE ) ) );
    };

  } else if( method == 'Block' ) {
    sim.rtns = bootstrap.returns( levels, nday.sim = ndays, block.size = block.size, num.sims = num.sims );
  } else if( method == 'Markov' ) {
    if( file.exists( get.simulation.filename( symbol, 1 ) ) && block.size == 1 ) {
       state    = ifelse( initial.state == -1, find.vix.state(), initial.state );
       sim.rtns = -1 + read.distribution.data( symbol, state, ndays );
    } else {
      if( identical( levels, c() ) && symbol != '' ) {
        levels = get.time.series( symbol, start.date = today() %m-% years(15), use.proxy = TRUE );
      };

      sim.levels = markov.vix.bootstrap( levels, nday.sim = ndays, block.size = block.size, num.sims = num.sims, output.all = FALSE );
      sim.rtns   = -1 + sim.levels;
    };
  } else {
    stop( paste( "Unsupported method:", method ) );
  };

  rownames( sim.rtns ) = NULL;
  return( sim.rtns );
};

###########################################################################################
# Routine: simulate.option.prices
#
# Given option parameters, calclate the distribution of future option prices
###########################################################################################

simulate.option.prices <- function( 
  levels, 
  symbol = '', 
  nday.sim, 
  maturity, 
  strike, 
  spot = 1, 
  opt.type = c( 'P', 'C' ) , 
  initial.state = -1
) {
  state = ifelse( initial.state == -1, find.vix.state(), initial.state );

  sim.levels = simulate.from.blocks( levels = levels, initial.state = state, nday.sim = nday.sim )[ nday.sim, ];

  final.states = find.block.states( initial.state = state, nday.sim = nday.sim );

  option.prices <- sim.levels  * NA;

  all.states = sort( unique( final.states ) );
  for( s in all.states ) {
    print( s );
    option.prices[ final.states == s ] = calc.option.prices( levels = levels, spot = spot * sim.levels[ final.states == s ], 
				initial.state = s,  nbiz.days = maturity - nday.sim, strikes = strike, put.call = opt.type )$Price;
  };
  
  return( option.prices ); 
};
  

###########################################################################################
# Routine: create.simulation.blocks
#
# Outputs a set of date numbers representing which day's returns should be used
###########################################################################################

create.simulation.blocks <- function( 
  initial.state, 
  start.date    = as.Date( '2004-3-31' ), 
  block.size    = MARKOV_BLOCK_SIZE, 
  nday.sim      = 600, 
  num.sims      = 1e4
) {
  VXO = get.time.series( '^VXO', start.date = start.date );

  levels = timeSeries( 1:nrow( VXO ), rownames( VXO ) ); 

  markov.output = markov.vix.bootstrap( levels, block.size = block.size, nday.sim = nday.sim, num.sims = num.sims, 
										output.all = TRUE, initial.state = initial.state );
 
  markov.output = rbind( rep( 1, num.sims ), markov.output );
  blocks        = apply( apply( markov.output, 2, ri ), 2, function(x) as.integer( round( 1 / x ) ) )

  block_dates   = matrix( as.numeric( as.Date( rownames( VXO ) ) )[ blocks ], ncol = num.sims );
 
  return( block_dates ); 
};

###########################################################################################
# Routine: save.simulation.blocks
#
# Save simulated blocks to .csv files
###########################################################################################

save.simulation.blocks <- function( 
  start.date    = as.Date( '2004-3-31' ),
  block.size    = MARKOV_BLOCK_SIZE,
  nday.sim      = 600,
  num.sims      = 1e4
) {

  for( initial.state in 1:10 ) {
     filename = sprintf( "%s/simulations/blocks_%d.csv", DATA_PATH, initial.state );

     print( paste( 'Writing blocks for initial state', initial.state ) );
     blocks = create.simulation.blocks( initial.state, start.date = start.date, block.size = block.size, 
										nday.sim = nday.sim, num.sims = num.sims );
    
     write.table( blocks, file = filename, row.names = F, col.names = F, sep = '\t' );
  };
};

###########################################################################################
# Routine: read.simulation.blocks
#
# Read simulated blocks from .csv files
###########################################################################################

read.simulation.blocks <- function( initial.state, nday.sim = -1 ) {
  filename = get.simulation.block.filename( initial.state );

  blocks = read.matrix( filename, sep = '\t' );

  if( nday.sim != -1 )
    blocks = blocks[ 1:max( nday.sim ), ];

  return( blocks );
};

###########################################################################################
# Routine: get.simulation.block.filename
###########################################################################################

get.simulation.block.filename <- function( initial.state ) {
  filename = sprintf( "%s/simulations/blocks_%d.csv", DATA_PATH, initial.state );
  return( filename );
};

###########################################################################################
# Routine: simulate.from.blocks
#
# Using the saved simulation sequence blocks, construct time series from levels
###########################################################################################

simulate.from.blocks <- function( 
  levels, 
  initial.state, 
  nday.sim = -1 
) {
  daily.rtns <- ri( levels );
  
  dates         <- as.Date( read.simulation.blocks( initial.state, nday.sim = nday.sim ) );
  missing.dates <- setdiff( unique( dates ), as.Date( rownames( daily.rtns ) ) );

  print( paste( 'Missing dates:', length( missing.dates ) ) );

  if( length( missing.dates ) / length( unique( dates ) ) > .03 )
    stop( 'Too many missing dates from levels in simulate.from.blocks' ); 

  if( length( missing.dates ) > 0 ) {
    fillNA     <- timeSeries( rep( 0, length( missing.dates ) ), as.Date( missing.dates ), units = colnames( daily.rtns ) ); 
    daily.rtns <- merge( daily.rtns, fillNA );
  };

  sim.rtns   <- matrix( daily.rtns[ dates, ], ncol = ncol( dates ), nrow = nrow( dates ) );
  sim.levels <- matrix( apply( 1 + sim.rtns, 2, cumprod ), ncol = ncol( dates ), nrow = nrow( dates ) );

  return( sim.levels );
}

###########################################################################################
# Routine: find.block.states
#
# For a given initial state, find the block numbers of the simulations ndays in the future
###########################################################################################

find.block.states <- function( 
  initial.state, 
  nday.sim
) {
  block.filename = get.simulation.block.filename( initial.state );
  output = read.simulation.data( block.filename, nday.sim = nday.sim );
  states = find.vix.state( date = as.Date( output ) )
  return( states );
};

###########################################################################################
# Routine: save.distribution.data
#
# Save distribution data into .csv files for distributions
###########################################################################################

save.distribution.data <- function( 
  symbol, 
  levels, 
  block.size      = MARKOV_BLOCK_SIZE,
  num.sims        = 2e4,
  num.buckets     = 10,
  nday.sim        = 600,
  prob.step.size  = 1e-3, 
  sim.from.blocks = TRUE
) {

  for( initial.state in 1:num.buckets ) {
    print( paste( symbol, initial.state ) );

    filename = get.simulation.filename( symbol, initial.state );

    if( sim.from.blocks == TRUE ) {
       sim.levels = simulate.from.blocks( levels, initial.state, nday.sim = nday.sim );
    } else {
       sim.levels = markov.vix.bootstrap( levels, nday.sim = 1:nday.sim, block.size = block.size, num.sims = num.sims, 
							num.buckets = num.buckets, initial.state = initial.state );
    };

    cum.probs = seq( from = 0, to = 1 - prob.step.size, by = prob.step.size );
    strikes   = apply( sim.levels, 1, quantile, cum.probs );

    # Transpose the data frame before writing, so that each row represents a different maturity
    write.table( t( strikes ), file = filename, row.names = F, col.names = F, sep = '\t' );
  };
};

###########################################################################################
# Routine: read.distribution.data
#
# Read distribution data from .csv files for distributions
###########################################################################################

read.distribution.data <- function( 
  symbol, 
  initial.state, 
  nday.sim = c()
) {
  filename = get.simulation.filename( symbol, initial.state );
  if( !file.exists( filename ) )
     return( c() );

  if( identical( nday.sim, c() )) {  
     output = read.matrix( filename, sep = '\t', header = FALSE );
     output = matrix( output, ncol = ncol( output ), nrow = nrow( output ) );
  } else {
     output = read.simulation.data( filename = filename, nday.sim = nday.sim );
  };

  return( output );
};


###########################################################################################
# Routine: read.simulation.data
###########################################################################################

read.simulation.data <- function( 
  filename, 
  nday.sim
) {
  output = c();

  for( nday in nday.sim ) {
     if( nday > 0 ) {
         output = rbind( output, scan( filename, skip = nday - 1, nlines = 1, quiet = TRUE ) );
     } else {
         output = rbind( output, rep( 1, length( scan( filename, skip = 0, nlines = 1, quiet = TRUE ) ) ) );
     };
   };

   return( output );
};


###########################################################################################
# Routine: get.simulation.filename
###########################################################################################

get.simulation.filename <- function( symbol, initial.state ) {
  filename = sprintf( "%s/simulations/%s_%d.csv", DATA_PATH, symbol, initial.state );
  return( filename );
}

##########################################################################################################
# Routine: garch.simulate
#
# Simulate paths using a GARCH( 1, 1 ) model, with the formula
#    \sigma_t^2 = \alpha_0 + \alpha_1 * \sigma_{t-1}^2 * \epsilon_t^2 + \beta_1 * \sigma_{t-1}^1
#
# where \epsilon_{t-1} is drawn from a N(0,1) distribution.  
##########################################################################################################

garch.simulate <- function(
  levels,
  nday.sim = 21, 
  end.date = last.date( levels ), 
  num.sims = 1
) {
  rtns = returns( levels );
  garch.output = garchFit( data = rtns );

  rands    = matrix( rnorm( nday.sim * num.sims, 0, 1 ), nrow = nday.sim );
  sim.rtns = matrix( NA, nrow = nday.sim, ncol = num.sims );
  sigma    = matrix( NA, nrow = nday.sim, ncol = num.sims );

  alpha_0 = coef( garch.output)[ 'omega' ];
  alpha_1 = coef( garch.output)[ 'alpha1' ];
  beta_1  = coef( garch.output)[ 'beta1' ]; 
  
  if( alpha_1 + beta_1 > 1 ) {
    stop( 'GARCH coefficients sum to more than one!  Volatility is non-stationary.' );
  };
  
  sigma_0 = fitted.values( garch.output )[ which( end.date == rownames( rtns ) ), 1 ]; 

  for( i in 1:nday.sim ) {
    if( i == 1 ) {
      sigma.conditional = rep( sigma_0, num.sims );
    } else {
      sigma[ i-1, ];
    };

    sim.rtns[ i, ] = sigma.conditional * rands[ i, ]
    sigma[ i, ] = sqrt( alpha_0 + alpha_1 * sim.rtns[ i, ] + beta_1 * sigma.conditional );
  };

  sim.levels <- matrix( apply( 1 + sim.rtns, 2, cumprod ), ncol = num.sims, nrow = nday.sim );
  return( sim.levels ); 
};



