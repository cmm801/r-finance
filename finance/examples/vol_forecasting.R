
test.vol.forecasts <- function( 
  N = 50, 
  methods = c( 'Standard', 'Parkinson', 'Rogers-Satchell', 'Garman-Klass', 'Yang-Zhang' ), 
  nDays = 1e4, 
  periodsPerDay = 1e3, 
  f  = .25, 
  MU = 0, 
  SD = .5
) {
  openInd  = 1
  closeInd = max( floor( (1-f) * periodsPerDay ), 1 );

  MU = .0;
  SD = .5;

  rands = rnorm( nDays * periodsPerDay, 0, 1 ) * SD / sqrt( 252 * periodsPerDay ) + MU / ( 252 * periodsPerDay );

  levels = cumprod( exp( rands ) );

  levmat = matrix( levels, periodsPerDay, nDays );

  Open  = levmat[ openInd,  ];
  Close = levmat[ closeInd, ];
  
  High = apply( matrix( levmat[ openInd:closeInd, ], ncol = nDays ), 2, max );
  Low  = apply( matrix( levmat[ openInd:closeInd, ], ncol = nDays ), 2, min );

  dates = as.Date( today() - nDays:1 );
  data = data.frame( Date = dates, Open, Close, High, Low );

  vol.estim = data.frame( Actual = rep( SD, length( dates ) - 1 ) );
  for( i in 1:length( methods ) ) {
    tmp =  data.frame( sqrt( 252 ) * calc.vol( data, method = methods[i], N = N ) );
    colnames( tmp ) = methods[i];
    vol.estim = cbind( vol.estim, tmp );
  };

  return( vol.estim );
};

means  = data.frame();
stds   = data.frame();
stderr = data.frame();
f  = 0.25;
MU = 0.05;
SD = 0.20; 

for( N in (1:10)*5 ) {
  for( MU in (0:3) * .04 ) {
    for( f in (0:3) * .1 ) {
      vol.estim = test.vol.forecasts( f = f, N = N, MU = MU, SD = SD );

      label = c( N, MU, f );
      print( label )

      means  = rbind( means,  c( label, apply( ( vol.estim / vol.estim$Actual - 1 ) * 100, 2, mean, na.rm = T ) ) );
      stds   = rbind( stds,   c( label, apply( ( vol.estim / vol.estim$Actual - 1 ) * 100, 2,sd,    na.rm = T ) ) )
      stderr = rbind( stderr, c( label, sqrt( apply( ( vol.estim / vol.estim$Actual - 1 )^2, 2,mean,    na.rm = T ) ) * 100 ) )

      colnames( means )  = c( 'N', 'MU', 'f', 'Actual', methods );
      colnames( stds )   = c( 'N', 'MU', 'f', 'Actual', methods );
      colnames( stderr ) = c( 'N', 'MU', 'f', 'Actual', methods );
    }
  };
};

