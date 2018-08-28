
Symbols     = c( 'VXX', 'SPY', 'FAS', 'TNA' );
Start.dates = c( as.Date( '2004-3-31' ) );
block.sizes = c( 1 );
nday.sim   = 21;
num.sims   = 1e4;
state      = 10;

for( symbol in Symbols ) {

  levels = get.ts.or.proxy( symbol, start.date = today() - years(10) ); 

  sim.rtns   = simulate.returns( levels, ndays = nday.sim, symbol = symbol, initial.state = state )
  plot(  1:1e3, sim.rtns, type = 'l', col ='blue' )

  for( start.date in Start.dates ) {
    start.date = as.Date( start.date );

    for( block.size in block.sizes ) {

      #  sim.levels = simulate.from.blocks( levels = levels, initial.state = state, nday.sim = nday.sim )[ nday.sim, ];

      sim.levels = markov.vix.bootstrap( levels, nday.sim = nday.sim, block.size = block.size, num.sims = num.sims, output.all = FALSE, 
							initial.state = state, start.date = start.date );

      lines( 1:1e3, -1 + sort( sim.levels )[ seq( from = 1, to = 1e4, by = 10 ) ], col = 'red' );
    };
  };
};

