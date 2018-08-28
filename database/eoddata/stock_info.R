

get.HKSE.symbols <- function() {
  data = c();
  for( i in 0:9 ) {
    url = sprintf( 'http://eoddata.com/stocklist/HKEX/%d.htm', i );
    raw.data = readHTMLTable( url, stringsAsFactors = FALSE );
    if( !is.null( raw.data ) ) {
      new.data = raw.data[[6]][,1:2];
      new.data = cbind( new.data, data.frame( Exchange = 'HKSE' ) );
      colnames( new.data ) = c( 'Symbol', 'Name', 'Exchange' );
      data = rbind( data, new.data );
    };
  };
 
  data = unique( data );
  data$Symbol = paste( data$Symbol, 'HK', sep = '.' );
  return( data );
};

get.NYSE.symbols <- function() {
  data = c();
  for( i in LETTERS ) {
    url = sprintf( 'http://eoddata.com/stocklist/NYSE/%s.htm', i );
    raw.data = readHTMLTable( url, stringsAsFactors = FALSE );
    if( !is.null( raw.data ) ) {
      new.data = raw.data[[6]][,1:2];
      new.data = cbind( new.data, data.frame( Exchange = 'NYSE' ) );
      colnames( new.data ) = c( 'Symbol', 'Name', 'Exchange' );
      data = rbind( data, new.data );
    };
  };

  data = unique( data );
  return( data );
};

get.LSE.symbols <- function() {
  data = c();
  for( i in c( as.character(0:9), LETTERS ) ) {
    url = sprintf( 'http://eoddata.com/stocklist/LSE/%s.htm', i );
    raw.data = readHTMLTable( url, stringsAsFactors = FALSE );
    if( !is.null( raw.data ) ) {
      new.data = raw.data[[6]][,1:2];
      new.data = cbind( new.data, data.frame( Exchange = 'LSE' ) );
      colnames( new.data ) = c( 'Symbol', 'Name', 'Exchange' );
      data = rbind( data, new.data );
    };
  };

  data = unique( data );
  data$Symbol = paste( data$Symbol, 'L', sep = '.' );
  return( data );
};

      'http://eoddata.com/Data/symbollist.aspx?e=AMEX'
url = 'http://eoddata.com/Data/symbollist.aspx?e=AMEX';
amex.data = read.csv( url, stringsAsFactors = FALSE );



