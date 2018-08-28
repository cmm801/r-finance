
N = 1000000;
T = 2;
mu = c( 0.08, .04 )
sigma = c( 0.16, .05 );

ptf_rtns = list();
ptf_rtns2 = list();
for( i in 0:10 ) {
  rtns = list();
  rtns_E = matrix( rnorm( N*T, mu[1], sigma[1] ), T, N );
  rtns_B = matrix( rnorm( N*T, mu[2], sigma[2] ), T, N );

  w = i/10;
  ptf_rtns[[i+1]] = w * rtns_E + (1-w) * rtns_B;
};

vardiff = c();
rtndiff = c();
for( i in 2:10 ) {
  rtndiff[i] = mean( ptf_rtns[[i+1]][1,] + ptf_rtns[[i-1]][2,] ) - mean( ptf_rtns[[i]][1,] + ptf_rtns[[i]][2,] )
  vardiff[i] = quantile( ptf_rtns[[i+1]][1,] + ptf_rtns[[i-1]][2,], .05 ) - quantile( ptf_rtns[[i]][1,] + ptf_rtns[[i]][2,], .05 );
};
