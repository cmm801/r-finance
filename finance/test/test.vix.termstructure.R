

plot.vix.term.structure <- function( as.percentage = TRUE ) {
  data.type = 'Settle';
  rm.zeros = TRUE;
  tenors = seq( from = 0, to = 360, by = 30 );

  raw.data = get.futures.data();
  vix = get.time.series( '^VIX', start.date = as.Date( min( raw.data$Trade_Date ) ) );
  vix.df = data.frame( Trade_Date = as.Date( rownames( vix ) ), Values = as.numeric( vix ), Days_to_Expiry = 0 );
  colnames( vix.df )[2] = data.type;

  # Remove VIX data on Settlement dates, when there is Futures data with Days_to_Expiry = 0
  expiry.dates = raw.data[ raw.data$Days_to_Expiry == 0, 'Trade_Date'  ];
  vix.df = vix.df[ !( vix.df$Trade_Date %in% expiry.dates ), ];

  data = rbind( raw.data[, colnames( vix.df ) ], vix.df );

  diffs = list();
  stds = c();
  medians = c();
  for( k in 1:240 ) {
    dates = data[ which( data[,'Days_to_Expiry' ] == k ), 'Trade_Date' ];

    dt0 = data[ data$Trade_Date %in% dates & data$Days_to_Expiry == 0, ]
    dtk = data[ data$Trade_Date %in% dates & data$Days_to_Expiry == k, ]

    if( as.percentage ) {
      diffs[[k]] = -1 + dtk[ order( dtk$Trade_Date ), ]$Settle / dt0[ order( dt0$Trade_Date ), ]$Settle;
    } else {
      diffs[[k]] = dtk[ order( dtk$Trade_Date ), ]$Settle - dt0[ order( dt0$Trade_Date ), ]$Settle;
    };

    if( length( diffs[[k]] ) > 30 ) {
      stds[k] = sd(diffs[[k]] );
      medians[k] = median(diffs[[k]] );
    };
  };

  par( mfrow = c(2,1) );
  plot( medians, xlab = 'Days to Expiry', ylab = 'Median Difference' );
  lines( ksmooth( which( !is.na( medians ) ), medians[ !is.na( medians ) ], kernel = 'normal', bandwidth = 21 ), col = 'red' )
  title( 'Median Difference between VIX Futures and VIX Spot' );

  plot( stds, xlab = 'Days to Expiry', ylab = 'Std. Dev. of Difference' );
  lines( ksmooth( which( !is.na( stds ) ), stds[ !is.na( stds ) ], kernel = 'normal', bandwidth = 21 ), col = 'red' )
  title( 'Std Dev of Difference between VIX Futures and VIX Spot' );
};









