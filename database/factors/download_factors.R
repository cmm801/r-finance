
##########################################################################################################
# Routine: upload.stambaugh.liquidity.factor
##########################################################################################################

upload.stambaugh.liquidity.factor <- function() {
  liq.ts = get.stambaugh.liquidity.factor();
  upload.factor( liq.ts, freq = 'M', shortName = 'LIQ' );
};

##########################################################################################################
# Routine: get.stambaugh.liquidity.factor
##########################################################################################################

get.stambaugh.liquidity.factor <- function() {
  url = 'http://finance.wharton.upenn.edu/~stambaugh/liq_data_1962_2011.txt'
  raw <- read.delim( url, sep="\t", fill=TRUE, header=FALSE, as.is=TRUE)
  first.row = min( which( !is.na( raw[,4] ) & raw[,4] != '-99' ) );
  yearMonths = as.numeric( raw[ first.row:nrow( raw ), 1 ] );
  monthEnds = yearMonth2date( yearMonths );
  values = as.numeric( raw[ first.row:nrow( raw ), 4 ] );
  liq.ts = timeSeries( values, monthEnds, 'LIQ' )
  return( liq.ts );
};

##########################################################################################################
# Routine: upload.ff.data
#
# Combine Fama French timeSeries into a single object, and save them into a csv file
##########################################################################################################

upload.ff.data <- function() {
  files <- list(
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors.zip', freq = 'M', 
			incols = c('Mkt-RF', 'SMB', 'HML', 'RF' ), outcols = c( 'MKT', 'SMB', 'HML', 'RF' ) ), 
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor.zip', freq = 'M', 
			incols = 'Mom', outcols = 'MOM' ),
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_ST_Reversal_Factor.zip', freq = 'M', 
			incols = 'ST_Rev', outcols = 'STRev' ),
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_LT_Reversal_Factor.zip', freq = 'M', 
			incols = 'LT_Rev', outcols = 'LTRev' ), 
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily.zip', freq = 'D', 
			incols = c('Mkt-RF', 'SMB', 'HML', 'RF' ), outcols = c( 'MKT', 'SMB', 'HML', 'RF' ) ), 
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_daily.zip', freq = 'D', 
			incols = 'UMD', outcols = 'MOM' ),
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_ST_Reversal_Factor_daily.zip', freq = 'D', 
			incols = 'UMD', outcols = 'STRev' ),
    list( filename = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_LT_Reversal_Factor_daily.zip', freq = 'D', 
			incols = 'UMD', outcols = 'LTRev' ) );

  for( i in 1:length( files ) ) {
    url = files[[i]]$filename;
    freq = files[[i]]$freq;
    incols = files[[i]]$incols;
    outcols = files[[i]]$outcols;

    new.ts <- download.ff.data( url, freq = freq, incols = incols, outcols = outcols );
    for( c in 1:ncol( new.ts ) ) {
      upload.factor( new.ts[, c ], freq = freq, shortName = colnames( new.ts[,c] ) );
    };
  }
};

##########################################################################################################
# Routine: download.ff.data
#
# Download and format Fama French factor data.  Returns a timeSeries object
##########################################################################################################

download.ff.data <- function( 
  url, 
  freq = 'D', 
  incols = c(), 
  outcols = c() 
) {
  # Copy Zip file from website into /tmp drive, unzip, and then read contents into variable 'txt'
  split.path = strsplit( url, '/' )[[1]];


  zip.filename = '/tmp/FF_Factors.zip'
  download.file( url, zip.filename );
  unzip( zip.filename, exdir = '/tmp' );
  
  txt.filename = paste( '/tmp/', gsub( '.zip', '.txt',  split.path[ grep( '.zip', split.path ) ] ), sep = '' );
  if( !file.exists( txt.filename ) ) {
    txt.filename = paste( '/tmp/', gsub( '.zip', '.TXT',  split.path[ grep( '.zip', split.path ) ] ), sep = '' );
  };
  
  txt = read.csv( txt.filename, stringsAsFactors = FALSE, header = FALSE );

  # Parse words on each row, and find first row that starts with a number, representing a year/month.  The last row
  #  is the first time the first column does not represent a year month after the first row
  parsed.data <- apply( txt, 1, function(x) as.numeric( strsplit( x, ' ' )[[1]][1] )  );
  first.row   <- min( which( !is.na( parsed.data ) ) );
  last.row    <- min( which( is.na( parsed.data[ -(1:first.row) ] ) ) ) + first.row - 1;

  # Get column names
  raw.colnames = strsplit( as.character( txt[ first.row-1, ] ), ' ' )[[1]];
  raw.colnames = raw.colnames[ raw.colnames != '' ]
  if( length( incols ) > 0 && length( outcols ) > 0 ) {
    colnames = outcols[ match( incols, raw.colnames ) ];
  } else {
    colnames = gsub( '\\-', '_', raw.colnames );
  };

  # Put data into matrix
  data <- c();
  for( i in first.row:last.row ) {
    split  = strsplit( as.character( txt[ i, ] ), ' ' )[[1]];
    nums  = as.numeric( split[ split != '' ] );
    data <- rbind( data, nums ); 
  };
 
  if( freq == 'M' ) {
    dates = yearMonth2date( data[, 1 ], method = 'last' );
  } else if ( freq == 'D' ) {
    dates = as.Date( sprintf( '%d-%d-%d', floor( data[, 1 ] / 1e4 ), floor(data[,1]/100) %% 100, data[,1] %% 100 ) );
  };

  ts = timeSeries( data[, -1 ], dates, units = colnames ) / 100;
  return( ts );
}; 


