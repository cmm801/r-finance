
source( paste( DATABASE_PATH, "/yahoo/generic.R", sep="" ) );
library( 'XML', quietly = TRUE );

###########################################################################################
# Routine: parse.gs.conviction.list.data
###########################################################################################

parse.gs.conviction.list.data <- function() {
  regions = c( 'Americas', 'Europe', 'Japan', 'Asia_Pacific', 'Australia_New_Zealand' );
  data = c();

  # Combine all regional HTML files into a single data frame  
  for( region in regions ) {
    path = sprintf( '%s/gs_conviction_list/%s.html', DATA_PATH, region );
    regional.data = readHTMLTable( path, stringsAsFactors = FALSE )[[1]];
    cols = colnames( regional.data );

    if( 'Country' %in% cols ) {
      cols[cols=='Industry'] = 'Sector';
      colnames( regional.data ) = cols;
    } else {
      cols[cols=="Industry / Company Name"] = 'Company Name';
      colnames( regional.data ) = cols;
      regional.data = cbind( regional.data, data.frame( Country = '' ) );
    };
    
    data = rbind( data, cbind( regional.data, data.frame( Region = region ) ) );
  }; 

  # Parse the information in the data frame
  gs.ccys = gsub( '[0-9,.]', '', data$'Price Target' );
  ccy.mapping.path = sprintf( '%s/downloads/gs_conviction_list/currency_mapping.txt', DATA_PATH );
  ccy.mapping = read.table( ccy.mapping.path, stringsAsFactors = FALSE );
  currencies = ccy.mapping[ match( gs.ccys, ccy.mapping[,1] ), 2 ];
  data = cbind( data, data.frame( Currency = currencies ) ); 

  # Reformat Price Target and Last Close columns as numeric
  # Replace currency symbols in prices with blanks
  regex = paste( '[,', paste( ccy.mapping[,1], collapse = '' ), ']', sep = '' );
  price.targets = as.numeric( gsub( regex, '', data$'Price Target' ) );
  price.targets[ currencies == 'GBP' ] = price.targets[ currencies == 'GBP' ]/100;
  data$'Price Target' = price.targets;

  last.closes = as.numeric( gsub( regex, '', data$'Last Close' ) );
  last.closes[ currencies == 'GBP' ] = last.closes[ currencies == 'GBP' ]/100;
  data$'Last Close' = last.closes;

  # UK stocks are quoted in pence, so divide by 100 to get GBP
  data$'Return Potential' = as.numeric( data$'Return Potential' ) / 100;

  # Format Date Added column as a proper date 
  dates = matrix(NA,nrow=nrow(data),ncol=1);
  for( i in 1:nrow(data) ) {
    raw.date = data[i,'Date Added'];
    month.name = substr( raw.date, 1, 3 );
    M = which( month.abb == month.name );
    D = as.numeric( substr( raw.date, 5, 6 ) );
    Y = as.numeric( substr( raw.date, 8, 11 ) );
    dates[i] = sprintf( '%d-%d-%d', Y, M, D );
  };

  data$'Date Added' = as.Date( dates );
  return( data );
};

###########################################################################################
# Routine: read.gs.trade.data
###########################################################################################

read.gs.trade.data <- function() {
  data = read.csv( sprintf( '%s/downloads/gs_conviction_list/symbol_map.csv', DATA_PATH ) );
  symbols = unique( data[,'Yahoo.Symbol']);
  symbols = symbols[ symbols != '' ];
  quotes = get.multiple.quotes( symbols, from = today() - years(50) );

};

###########################################################################################
# Routine: create.gs.symbol.list
###########################################################################################

create.gs.symbol.list <- function() {
  data = read.csv( sprintf( '%s/downloads/gs_conviction_list/symbol_map.csv', DATA_PATH ) );
  symbols = unique( data[,'Yahoo.Symbol']);
  symbols = symbols[ symbols != '' ];

  equity.pathname = sprintf( '%s/calculated/symbol_lists/equity_symbol_lists', DATA_PATH );
  output.filename = sprintf( '%s/gs_conviction_equity_symbol_list.txt', equity.pathname );
  write.table( symbols, file = output.filename, row.names = FALSE, col.names = TRUE, quote = FALSE );
};


