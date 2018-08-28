
library( 'XML', quietly = TRUE );
library( 'RCurl', quietly = TRUE );

source( paste( DATABASE_PATH, "/database.R", sep="" ) );
options(stringsAsFactors = FALSE);

###########################################################################################
# Routine: download.index.constituents
# 
# Download equity index constituents
###########################################################################################

download.index.constituents <- function() {

  index.info.yahoo = download.index.constituents.yahoo();
  index.info.wiki  = download.index.constituents.wiki();

  index.info = rbind( index.info.yahoo, index.info.wiki );
  return( index.info );
};

###########################################################################################
# Routine: save.index.constituents
###########################################################################################

save.index.constituents <- function() {
  index.info = download.index.constituents();
  write.table( index.info, file = INDEX_CONSTITUENTS_TXT, sep="\t", row.names = FALSE, col.names = TRUE, quote = FALSE ); 
  nrows.added = nrow(index.info);
  return(nrows.added);
};

###########################################################################################
# Routine: get.index.constituents
###########################################################################################

get.index.constituents <- function( index.symbol = NA ) {
  index.info = read.delim( INDEX_CONSTITUENTS_TXT, header = TRUE, stringsAsFactors = FALSE );

  if( !is.na( index.symbol ) ) {
    index.info = index.info[ index.info$Index == index.symbol, ];
    return( index.info$Symbol );
  } else {
    return( index.info );
  };
};

###########################################################################################
# Routine: download.index.constituents.yahoo
# 
# Download equity index constituents
###########################################################################################

download.index.constituents.yahoo <- function() {
  #define parts of the URL

  index.list = read.table( INDEX_LIST_TXT, sep = '\t', header = TRUE );
  index.list = index.list[ index.list$Data.Source == 'Yahoo', ];

  index.info = c();
  for( i in 1:nrow( index.list ) ) {
    index.symbol = index.list$Symbol[i];

    k = 0;
    success = TRUE;
    while( success ) { 

      url = sprintf( 'http://finance.yahoo.com/q/cp?s=%s&c=%d', index.symbol, k );
      data = try( readHTMLTable( getURL(url), stringsAsFactors = FALSE ), silent = TRUE );

      if( class( data ) == 'try-error' || is.null( data[[9]] ) ) {
        success = FALSE;
      } else {   
        new.data = cbind( data.frame( Index = index.symbol ), data[[9]][,1:2] );
        new.data = new.data[ new.data$Symbol != '', ];
        index.info = rbind( index.info, new.data );
      };

      k = k+1;
    };
  };
 
  return( index.info );
}  

###########################################################################################
# Routine: download.index.constituentuents.wiki
# 
# Download equity index constituents
###########################################################################################

download.index.constituents.wiki <- function() {
  index.list = read.table( INDEX_LIST_TXT, sep = '\t', header = TRUE );
  index.list = index.list[ index.list$Data.Source == 'Wikipedia', ];
  indices = index.list$Symbol;

  output = c();
  for( i in 1:length(indices) ) {
    index = indices[i];
    if( '^GSPC' == index ) {
      url = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies";
      tabs = getURL( url );
      raw.data = readHTMLTable( tabs, stringsAsFactors = FALSE );
      sp.info = raw.data[[1]];

      sp.output = data.frame( Index = '^GSPC', Symbol = sp.info$'Ticker symbol', Name = sp.info$Security );
      output = rbind( output, sp.output );
    } else {
      stop( sprintf( 'Unsupported index: %s', index ) );
    };
    
  };

  return( output );
};


###########################################################################################
# Routine: download.index.constituents.sp
# 
# DEPRECATED - S&P doesn't offer this information any more
# Download equity index constituents
###########################################################################################

download.index.constituents.sp <- function() {
  index.list = read.table( INDEX_LIST_TXT, sep = '\t', header = TRUE );
  index.list = index.list[ index.list$Data.Source == 'SP', ];

  require(gdata);
  installXLSXsupport();

  base.url = paste( c( 'http://us.spindices.com/idsexport/file.xls?hostIdentifier=48190c8c-42c4-46af-8d1a-0cd5db894797&',
		  'selectedModule=Constituents&selectedSubModule=ConstituentsFullList&indexId=' ), collapse = '' );

  index.info = c();
  for( i in 1:nrow( index.list ) ) {
    url = sprintf( '%s%s', base.url, index.list[i,'Code'] );
    index.symbol = index.list[i,'Symbol'];

    data = read.xls( url, sheet = 'Constituents' );
    symbol.data = data[[2]];

    loc = which( symbol.data == 'Symbol' );
    symbols = setdiff( symbol.data[-(1:loc)], '' );
    index.info = rbind( index.info, data.frame( Index = index.symbol, Symbol = symbols, Name = 'NA' ) );
  };

  return( index.info );
};



