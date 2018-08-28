
##########################################################################################################
# Routine: get.schiller.data
##########################################################################################################

get.schiller.data <- function( ) {
  filename = sprintf( '%s/downloads/SchillerData.csv', DATA_PATH );
  data = read.csv( filename, stringsAsFactors = FALSE );
  data = data[ apply( is.na( data ), 1, sum ) !=  ncol( data ), ];
   
  num = data[,-1];
  yearMonths = data[,1] * 100;
  monthEnds = yearMonth2date( yearMonths );
  ts = timeSeries( num, monthEnds );
  return( ts );
};

