
##########################################################################################################
# Routine: update.db.factors
##########################################################################################################

update.db.factors <- function() {
  # Update Fama-French Factors
  upload.ff.data();

  # Monthly Stambaugh Liquidity Factor
  upload.stambaugh.liquidity.factor();
};

##########################################################################################################
# Routine: upload.factor
##########################################################################################################

upload.factor <- function(
  factor.ts,
  seriesID = NA,
  freq = NA,
  shortName = NA,
  tolerance = 1e-4, 
  Table = 'TSData', 
  DB = MYSQL_DB_NAME
) {
  # Find the time series ID for a given shortName and frequency
  info = find.factor.info( freq = freq, shortName = shortName, seriesID = seriesID );
  if( nrow( info ) == 1 ) {
      # Get existing db data for comparison
      db.ts = try( get.factor.ts( shortName, freq = freq, start.date = today() - years(200) ), silent = TRUE );

      # If db.ts is a time series, than compare existing database data to the new data on overlapping region 
      if( is.timeSeries( db.ts ) ) {
        overlap.ts = ts.range( factor.ts, end.date = last.date( db.ts ) );
        new.ts  = ts.range( factor.ts, start.date = last.date( db.ts ) + 1 );

        # Check that overlapping data does not disagree by too much 
        ts = nanmerge( overlap.ts, factor.ts );
        differences = abs( as.numeric( ts[,1] ) - as.numeric( ts[,2] ) );
        if( max( differences ) > tolerance ) {
          stop( sprintf( 'Factor %s database data differs from data source by more than the tolerance', shortName ) );
        };
      } else {
        # If db.ts is not a time series, than all data can just be added to the database
        new.ts = factor.ts
      };

      if( nrow( new.ts ) > 0 ) {
        data = data.frame( seriesID = info$seriesID, Date = as.Date( rownames( new.ts ) ), Value = as.numeric( new.ts ) );
        upload.data( data, DB = DB, Table = Table );
      };
  } else {
    error( 'The factor information did not match anything in the database' );
  };
};

##########################################################################################################
# Routine: upload.stambaugh.liquidity.factor
##########################################################################################################

upload.stambaugh.liquidity.factor <- function() {
  liq.ts = get.stambaugh.liquidity.factor();
  upload.factor( liq.ts, freq = 'M', shortName = 'LIQ' );
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


