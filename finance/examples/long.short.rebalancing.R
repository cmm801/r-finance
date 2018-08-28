
index = '^GSPC';
leverage = 3;
start.date = as.Date( '1970-01-01' );
weights   = c( 2, -1/3, -2/3 );
alpha     = -1e-4;

underlier = get.time.series( index, start.date = start.date, data.type = 'Close' );
long  = prod.ts(  leverage * ri( underlier ) + alpha )
short = prod.ts( -leverage * ri( underlier ) + alpha )
USD   = get.cash.ts();

colnames( long )  = 'Long';
colnames( short ) = 'Short';

levels = merge.ts( list( USD, long, short ) );
levels = levels[ !is.na( rowSums( levels ) ), ];

returns = ri( levels );

mv = matrix( NaN, nrow = nrow( levels ), ncol = ncol( levels ) );
mv[ 1, ] = weights;
for( i in  2:nrow( levels ) ) {
  mv[ i, ] = mv[ i - 1, ] * ( 1 + returns[i-1, ] );

  if( sum( mv[ i, ] ) < 0 ) {
    mv[ i, ] = weights * 0
  };

  if( mv[i, 2] / mv[ i, 3 ] > 1 | mv[ i, 2 ] / mv[ i, 3 ] < .5 ) {
     mv[ i, ] = weights * sum( mv[ i, ] );
  };
};

strat = timeSeries( rowSums( mv ), rownames( levels ) );

