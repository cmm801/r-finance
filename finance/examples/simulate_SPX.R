
start.date = as.Date( '1960-01-01' );
end.date = as.Date( '2012-12-20' );

nDayPrediction = 10;

ts = get.time.series( '^GSPC', start.date = start.date, end.date = end.date );
daily.rtns = returns( ts );
rtns = ri( ts, nDayPrediction );

# Estimate distribution assuming normality for 3m, 6m, 1y, 2y, 5y, 10y
# windowSize = c( 63, 126, 252, 504, 1260, 2520 );
windowSize = 1260;

avg.rtns = lapply( windowSize, function(ndays) SMA( rtns, ndays ) )
avg.sd   = lapply( windowSize, function(ndays) rolling.sd.rtns( rtns, ndays ) / sqrt( 252 ) ) 

probs = list();
for( w in 1:length( windowSize ) ) {
  tmp = merge.ts( list( avg.rtns[[w]], avg.sd[[w]], lag( rtns, -nDayPrediction ) ) );
  colnames( tmp ) = c( 'Mean', 'StdDev', 'Lag' );
  tmp = na.omit( tmp );

  probs[[w]] = tmp[,1] * NaN;
  for( r in 1:( length(dates) - nDayPrediction ) ) {
    probs[[w]][r,] = pnorm( tmp[ r, 'Lag' ], tmp[ r, 'Mean' ], tmp[ r, 'StdDev' ] );
  }
};

tprobs = list();
for( w in 1:length( windowSize ) ) {
  tmp = merge.ts( list( avg.rtns[[w]], avg.sd[[w]], lag( rtns, -nDayPrediction ) ) );
  colnames( tmp ) = c( 'Mean', 'StdDev', 'Lag' );
  tmp = na.omit( tmp );

  tprobs[[w]] = tmp[,1] * NaN;
  for( r in 1:( length(dates) - nDayPrediction ) ) {
    tprobs[[w]][r,] = pnorm( tmp[ r, 'Lag' ], tmp[ r, 'Mean' ], tmp[ r, 'StdDev' ] );
  }
};




