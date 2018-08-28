
ust = get.fed.data( c( 'DTB3', 'DGS1', 'DGS5', 'DGS10' ) ) / 100;
ust = na.omit( ust );
ust[ -which( apply( as.matrix( ust ), 1, prod ) == 0 ), ]

cash.vec = c( 1, 0, 0, 0 );
lev.vec = c( 0,  0.5, 0, 0.5 );
slp.vec = c( 0, -0.5, 0, 0.5 );
con.vec = c( 0, 0.25, -0.50, 0.25 );

shape.vec = rbind( cash.vec, lev.vec, slp.vec, con.vec );
shape.ts = as.timeSeries( ust %*% t( shape.vec ) );
rownames( shape.ts ) = rownames( ust );

