
###########################################################################################
# Routine: screen.all.option.prices
#
# Go through the available tickers and output a list of the best values
###########################################################################################

screen.all.option.prices <- function() {
  all.symbols = last.db.date();

  Symbols = all.symbols[ all.symbols$Date == max( all.symbols$Date ), 'Symbol' ];

  output = data.frame();
  for( symbol in Symbols ) {

    if( sum( grep( '\\^', symbol ) ) != 0 )
       next;
  
    print( symbol );

    output = try( rbind( output, screen.option.prices( symbol, rank.col = 'Annualized.Return', num.outputs = 30 ) ) );
  };

  output = unique( output );

  write.table( output, file = sprintf( '%s/data/option_opportunities.csv', BASE_PATH ), sep = '\t', row.names = FALSE, quote = FALSE )

  return( output );
};

