

###########################################################################################
# Routine: plot.colors
# 
# Get a list of distinct colors for plotting
###########################################################################################

plot.colors <- function() {
  pc = c( 'black', 'blue', 'red', 'darkmagenta', 'antiquewhite4', 'darkgreen', 'cyan4', 'darkorange1', 'darkorange4', 
	  'darkorchid4', 'darkolivegreen3', 'darkgoldenrod', 'chocolate', 'darkred', 'darkblue', 'forestgreen' );
  return( pc )
};

###########################################################################################
# Routine: plot.weights
# 
# Plot a filled square representing weights of each asset
###########################################################################################

plot.weights <- function( weights, names = c() ) {
  N = nrow( weights );
  cumwts = cbind( rep( 0, N ), t( apply( weights, 1, cumsum ) ) );

  plot( 1:N, cumwts[, 1 ], ylim = c( 0, max( cumwts ) ), col = 1, type = 'l' );
  for( i in 2:ncol( cumwts ) ) {
     points( 1:N, cumwts[, i ], col = 'black', type = 'l' );
     polygon( c( 1:N, N:1 ), c( cumwts[, i ], rev( cumwts[, i-1 ] ) ), border = FALSE, col = i-1 );
  };

  legend( "right", "top", legend = names, fill = 1:length( names ) );
};

###########################################################################################
# Routine: plot.many
#
# Plot multiple time series on the same graph
###########################################################################################

plot.many <- function( ts, legend.loc = 'topleft' ) {
  ylim = c( min( ts, na.rm = T ), max( ts, na.rm = T ) ) * 1.2;
  col = plot.colors();

  plot( ts[, 1 ], col = col[1], ylim = ylim, type = 'l' );
  for( c in 2:ncol( ts ) ) {
    lines( ts[, c ], col = col[c] );
  };

  legend( legend.loc, legend = colnames( ts ), fill = plot.colors()[ 1:ncol( ts ) ] )
};



