
T = 1;
M = 10;
roll = 0.001;

y1 = rep( 0.04, T+1 ); 
y0 = y1 - roll;
sigma = 0.01;

N = 100000;

base.rtns = rep( NA, T );
tot.rtns = matrix( NA, nrow = T, ncol = N );
conv.rtns = matrix( NA, nrow = T, ncol = N );

for( t in 1:T ) {
  y0_sim = rnorm( N, y0[t], sigma );

  P0 = 1/(1+y1[t])^M;
  P1 = 1/(1+y0[t])^(M-1);

  base.rtns[t] = -1 + P1 / P0; 

  for( n in 1:N ) {

    P0 = 1/(1+y1[t])^M;
    P1 = 1/(1+y0_sim[n])^(M-1);

    tot.rtns[t,n] = -1 + P1 / P0;
    conv.rtns[t,n] = tot.rtns[t,n] - base.rtns[t];
  }
};

avg.conv.rtns = -1 + mean( apply( conv.rtns, 2, function(x) prod( 1 + x ) )  );
avg.tot.rtns = -1 + mean( apply( tot.rtns, 2, function(x) prod( 1 + x ) )  );

