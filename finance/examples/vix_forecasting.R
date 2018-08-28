
T = 30;
Maturity = 60;
tenors = c( 0, 30, 60, 90, 120 );
fut.ts = get.constant.maturity.futures(tenors = tenors );

Y = fill.missing( timeSeries( as.numeric( fut.ts[,which(Maturity==tenors)] ), as.Date( rownames( fut.ts ) ) - days(T), units = 'FutureValue' ) );
Y = na.omit( Y[ match( rownames( fut.ts ), rownames( Y ) ),] );
X = fut.ts[ match( rownames( Y ), rownames( fut.ts ) ), ];

reg = lm( Y ~ X );


