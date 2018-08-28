symbol     = 'VXX';
nday.sim   = 21;
block.size = 5;
num.sims   = 1e4;

levels = get.ts.or.proxy( symbol, start.date = today() - years(10) );

for( state in 1:10 ) {
  print( state );

#  sim.levels = simulate.from.blocks( levels = levels, initial.state = state, nday.sim = nday.sim )[ nday.sim, ];
  sim.levels = markov.vix.bootstrap( levels, nday.sim = nday.sim, block.size = block.size, num.sims = num.sims, output.all = FALSE, initial.state = state, start.date = as.Date( '1986-1-1' ));

sim.rtns   = simulate.returns( levels, ndays = nday.sim, symbol = symbol, initial.state = state )

  if( state == 1 ) {
    plot( 1:1e3, sim.rtns, type = 'l', col ='blue' ) 
  } else {
    lines( 1:1e3, sim.rtns, type = 'l', col ='blue' )
  };

  lines( 1:1e3, -1 + sort( sim.levels )[ seq( from = 1, to = 1e4, by = 10 ) ], col = 'red' );
};

