
num.sims = 1e4;
nday.sim = 20;

SPXVIX = get.time.series( c( "^GSPC", "^VXO" ), start.date = today() - years( 30  ) );

SPX = SPXVIX[, 1 ];
VIX = SPXVIX[, 2 ];

rtns = ri( SPX );

bucket_vix = bucket.data( VIX, n = 10 );
one_bucket = bucket.data( VIX, n =  1 );

boot_locs_0 = markov.bootstrap( bucket_vix, num.blocks =  1, block.size = 20, num.sims = num.sims )
boot_locs_1 = markov.bootstrap( bucket_vix, num.blocks = 10, block.size =  2, num.sims = num.sims )
boot_locs_2 = markov.bootstrap( bucket_vix, num.blocks =  4, block.size =  5, num.sims = num.sims )
boot_locs_3 = markov.bootstrap( bucket_vix, num.blocks =  2, block.size = 10, num.sims = num.sims )
boot_locs_s = markov.bootstrap( one_bucket, num.blocks =  4, block.size =  5, num.sims = num.sims )

boot_rtns_0 = matrix( rtns[ boot_locs_0 ], ncol = ncol( boot_locs_0 ), byrow = FALSE );
boot_rtns_1 = matrix( rtns[ boot_locs_1 ], ncol = ncol( boot_locs_1 ), byrow = FALSE );
boot_rtns_2 = matrix( rtns[ boot_locs_2 ], ncol = ncol( boot_locs_2 ), byrow = FALSE );
boot_rtns_3 = matrix( rtns[ boot_locs_3 ], ncol = ncol( boot_locs_3 ), byrow = FALSE );
boot_rtns_s = matrix( rtns[ boot_locs_s ], ncol = ncol( boot_locs_s ), byrow = FALSE );

boot_rtns_0[ is.na( boot_rtns_0 ) ] = 0;
boot_rtns_1[ is.na( boot_rtns_1 ) ] = 0;
boot_rtns_2[ is.na( boot_rtns_2 ) ] = 0;
boot_rtns_3[ is.na( boot_rtns_3 ) ] = 0;
boot_rtns_s[ is.na( boot_rtns_s ) ] = 0;

breaks = c( seq( from = 0, to = .05, by = .0005 ), 1 );
hist_dat_0 = hist( sd( boot_rtns_0 ), breaks = breaks, plot = FALSE );
hist_dat_1 = hist( sd( boot_rtns_1 ), breaks = breaks, plot = FALSE );
hist_dat_2 = hist( sd( boot_rtns_2 ), breaks = breaks, plot = FALSE );
hist_dat_3 = hist( sd( boot_rtns_3 ), breaks = breaks, plot = FALSE );
hist_dat_s = hist( sd( boot_rtns_s ), breaks = breaks, plot = FALSE );

par( mfrow = c( 2, 2 ) )
plot(   hist_dat_0$breaks[-1], hist_dat_0$density, xlim = c( 0, .02 ), type = 'l', col = 'black', xlab = 'Stdev of Returns', ylab = 'Density' )
lines(  hist_dat_1$breaks[-1], hist_dat_1$density, col = 'blue' )
lines(  hist_dat_2$breaks[-1], hist_dat_2$density, col = 'red' )
lines(  hist_dat_3$breaks[-1], hist_dat_3$density, col = 'green' )
lines(  hist_dat_s$breaks[-1], hist_dat_s$density, col = 'black', lty = 2 )

plot(   hist_dat_0$breaks[-1], cumsum( hist_dat_0$density   ) , xlim = c( 0, .02 ), type = 'l', col = 'black', , xlab = 'Stdev of Returns', ylab = 'Cum Density' )
lines(  hist_dat_1$breaks[-1], cumsum( hist_dat_1$density ), col = 'blue' )
lines(  hist_dat_2$breaks[-1], cumsum( hist_dat_2$density ), col = 'red' )
lines(  hist_dat_3$breaks[-1], cumsum( hist_dat_3$density ), col = 'green' )
lines(  hist_dat_s$breaks[-1], cumsum( hist_dat_s$density ), col = 'black', lty = 2 )


levels_0 = apply( 1 + boot_rtns_0, 2, function(x) cumprod( x ) )
levels_1 = apply( 1 + boot_rtns_1, 2, function(x) cumprod( x ) )
levels_2 = apply( 1 + boot_rtns_2, 2, function(x) cumprod( x ) )
levels_3 = apply( 1 + boot_rtns_3, 2, function(x) cumprod( x ) )
levels_s = apply( 1 + boot_rtns_s, 2, function(x) cumprod( x ) )

breaks = seq( from = 0, to = 2, by = .001 );
hist_lev_0 = hist( levels_0[ nday.sim, ], breaks = breaks, plot = FALSE );
hist_lev_1 = hist( levels_1[ nday.sim, ], breaks = breaks, plot = FALSE );
hist_lev_2 = hist( levels_2[ nday.sim, ], breaks = breaks, plot = FALSE );
hist_lev_3 = hist( levels_3[ nday.sim, ], breaks = breaks, plot = FALSE );
hist_lev_s = hist( levels_s[ nday.sim, ], breaks = breaks, plot = FALSE );

plot( hist_lev_0$breaks[-1],  hist_lev_0$density, xlim = c( .9, 1.1 ), type = 'l', , xlab = 'Density of Returns', ylab = 'Density' )
lines( hist_lev_1$breaks[-1], hist_lev_1$density, col = 'blue' );
lines( hist_lev_2$breaks[-1], hist_lev_2$density, col = 'red' );
lines( hist_lev_3$breaks[-1], hist_lev_3$density, col = 'green' );
lines( hist_lev_s$breaks[-1], hist_lev_s$density, col = 'black', lty = 2 );

plot( hist_lev_0$breaks[-1],  cumsum( hist_lev_0$density ), xlim = c( .9, 1.1 ), type = 'l', , xlab = 'Density of Returns', ylab = 'Cum Density' )
lines( hist_lev_1$breaks[-1], cumsum( hist_lev_1$density ), col = 'blue' );
lines( hist_lev_2$breaks[-1], cumsum( hist_lev_2$density ), col = 'red' );
lines( hist_lev_3$breaks[-1], cumsum( hist_lev_3$density ), col = 'green' );
lines( hist_lev_s$breaks[-1], cumsum( hist_lev_s$density ), col = 'black', lty = 2 );


