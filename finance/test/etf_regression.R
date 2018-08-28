
numPeriods = 240;
numSims = 1000;
windowSize = 60;

sigma = c( .20, .15 ) / sqrt( 12 );
rho = .5;
correl = matrix( c( 1, rho, rho, 1 ), ncol = 2, nrow = 2 );

roll.betas = numeric( numSims ) * NaN;
tot.betas = numeric( numSims ) * NaN;

for( s in 1:numSims ) {
  tmp = 0;
  rands = chol.generate( numPeriods, correl, std = sigma );
  
  for( i in windowSize:numPeriods ) {
    Y = rands[(i-windowSize+1):i, 1 ];
    X = rands[(i-windowSize+1):i, 2 ];
 
    reg = lm( Y ~ X ); 
    tmp = tmp + as.numeric( reg$coefficients )[2];
  };
  
  roll.betas[s] = tmp / ( numPeriods - windowSize + 1 );
  tot.betas[s] = as.numeric( lm( rands[,1] ~ rands[,2] )$coefficients )[2];
};

