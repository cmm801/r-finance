vol = rolling.vol( SPX, 21 )
fut.rtn = lag( rolling.vol( SPX, 21 ), -21 );
ts = nanmerge( fut.rtn, vol )
names(ts) = c( 'FutVol', 'Vol' );
dts = as.data.frame( ts)

ddts = dts[ seq( 1, nrow(dts ), by = 21 ), ];

lw = lowess( ddts$Vol, ddts$FutVol );
plot( ddts$Vol, ddts$FutVol );
lines( lw, col = 'red' );


