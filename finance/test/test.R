asOfDate = as.Date( "2014-06-04" );
ticker = 'VXX';
yearMonth = 201501;
rf.rate = 0.001;
div.yield = 0;

ts = get.time.series( ticker, start.date = asOfDate - 3, end.date= asOfDate + 3 )
spot = as.numeric( ts[asOfDate] );

expiry = as.Date( get.expiries( 'US Options', start.ym = yearMonth, end.ym = yearMonth ) );
maturity = as.numeric( expiry ) - as.numeric( asOfDate );

F = spot * exp( rf.rate * maturity ); 

opt.data  = get.option.data( ticker, yearMonth, asOfDate = asOfDate );
opt.data$Mid = ( opt.data$Ask + opt.data$Bid ) / 2;

calls = opt.data[ opt.data$Type == 'C', ];
puts  = opt.data[ opt.data$Type == 'P', ];

opt.call = EuropeanCallBS( Spot = spot, Strike = calls$Strike, Maturity = maturity, 
                                DivYield = div.yield, RiskFreeRate = rf.rate, Price = calls$Mid );
opt.put  = EuropeanPutBS( Spot = spot, Strike = puts$Strike, Maturity = maturity,      
                                DivYield = div.yield, RiskFreeRate = rf.rate, Price = puts$Mid );

plot( log( opt.call@Strike / F ), calcImpliedVol( opt.call ) )
points( log( opt.put@Strike / F ), calcImpliedVol( opt.put ), col = 'red' )


