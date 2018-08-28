

fit.rt.vols <- function( df ) {
  lastdt = df[ df$Trade_Time == tail( sort( unique( df$Trade_Time ) ), 1 ), ];
  last.med.ask.vol = median( lastdt$AskVol, na.rm = T );
  last.med.bid.vol = median( lastdt$BidVol, na.rm = T );

  adjdt = lastdt;
  for( tt in 1:length( trade.times ) ) {
    trade.time = trade.times[tt];
    dtt = df[ df$Trade_Time == trade.time & df$nDays == maturities[nMat], ];
    common.strikes = intersect( dtt$Strike, lastdt$Strike );
    if( is.empty( dtt ) == 0 ) {
      next;
    };

    med.bid.vol=median( dtt[ dtt$Strike %in% common.strikes,]$BidVol / lastdt[ lastdt$Strike %in% common.strikes,]$BidVol , na.rm = T );
    med.ask.vol=median( dtt[ dtt$Strike %in% common.strikes,]$AskVol / lastdt[ lastdt$Strike %in% common.strikes,]$AskVol , na.rm = T );

    adtt = dtt;
    adtt$AskVol = adtt$AskVol / med.ask.vol;
    adjdt = rbind( adjdt, adtt );

    print( last.med.ask.vol / med.ask.vol )
  };

  strikes = sort( unique( adjdt$Strike ) )
  med.vols = unlist( lapply( strikes, function(x) median( adjdt[ adjdt$Strike == x, 'AskVol' ], na.rm = T ) ) )
  imp.vol = data.frame( ImpliedVol = med.vols, K = strikes / unique( lastdt$Spot ) );
  return( imp.vol );
};

start.time = as.POSIXlt( '2013-02-08 09:30 EST' );
end.time = as.POSIXlt( '2013-02-08 16:30 EST' );

divs = c( -.10, -.05, 0, .05 );
colors = c( 'red', 'blue', 'black', 'green' );
for( i in 1:length(divs) ) {
  ad = get.yahoo.rt.option.data( 'VXX', dividendYield = divs[i], start.time = start.time, end.time = end.time )

  nMat = 4;
  TT = 13;
  lambda = .1
  maturities = sort( unique( ad$nDays ) );
  trade.times = sort( unique( ad$Trade_Time ) );

  puts = ad[ ad$Type == 'P' & ad$nDays == maturities[nMat], ];
  calls = ad[ ad$Type == 'C' & ad$nDays == maturities[nMat], ];

  put.vols = fit.rt.vols( puts )
  call.vols = fit.rt.vols( calls )

  K = sort( unique( c( put.vols$K, call.vols$K ) ) );

  all.put.vols = unlist( lapply( K, function(k) approx( put.vols$K, put.vols$ImpliedVol, k )$y ) );
  all.call.vols = unlist( lapply( K, function(k) approx( call.vols$K, call.vols$ImpliedVol, k )$y ) );

  w = (  tanh( ( 1 - 1/K ) /.1 ) + 1  ) / 2;
  int.vols = (1-w) * all.put.vols + w*all.call.vols; 
  if( i == 1 ) {
    plot( K, int.vols, col = colors[i], type = 'l' );
  } else {
    lines( K, int.vols, col = colors[i] );
  };
};

