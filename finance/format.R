

##########################################################################################################
# Routine: get.format
#
# Get the format object of a given type
##########################################################################################################

get.format <- function( 
  format.type, 
  decimals = NULL, 
  as.percent = NULL, 
  output.string = TRUE 
) {
  if( format.type %in% c( 'string', 'char' ) ) {
    fmt.obj = get.format.string();
  } else if( is.null( format.type ) || is.empty( format.type ) ) {
    fmt.obj = get.format.Generic();
  } else if( format.type == 'Generic' ) {
    fmt.obj = get.format.Generic();
  } else if( format.type == 'double0' ) {
    fmt.obj = get.format.Generic( decimals = 0 );
  } else if( format.type == 'double1' ) {
    fmt.obj = get.format.Generic( decimals = 1 );
  } else if( format.type == 'double2' ) {
    fmt.obj = get.format.Generic( decimals = 2 );
  } else if( format.type == 'percent0' ) {
    fmt.obj = get.format.Generic( decimals = 0, AsPercent = TRUE );
  } else if( format.type == 'percent1' ) {
    fmt.obj = get.format.Generic( decimals = 1, AsPercent = TRUE );
  } else if( format.type == 'percent2' ) {
    fmt.obj = get.format.Generic( decimals = 2, AsPercent = TRUE );
  } else if( format.type %in% c( 'SharpeRatio', 'SR' ) ) {
    fmt.obj = get.format.SharpeRatio();
  } else if( format.type == 'Price' ) {
    fmt.obj = get.format.Price();
  } else if( format.type %in% c( 'Returns', 'Return', 'Percent' ) ) {
    fmt.obj = get.format.Return();
  } else if( format.type == 'Quantity' ) {
    fmt.obj = get.format.Quantity();
  } else if( format.type %in% c( 'MarketValue', 'MV' ) ) {
    fmt.obj = get.format.MarketValue();
  } else if( format.type == 'MMMYY' ) {
    fmt.obj = get.format.MMMYY();
  } else if( format.type == 'YYYYMMDD' ) {
    fmt.obj = get.format.YYYYMMDD();
  } else if( format.type == 'MMDDYYYY' ) {
    fmt.obj = get.format.MMDDYYYY();
  } else if( format.type == 'DDMMYYYY' ) {
    fmt.obj = get.format.DDMMYYYY();
  } else {
    stop( paste( 'Error: Unsupported format type: ', format.type ) );
  };

  # Specify whether the output should be in 'string' or 'numeric' format
  if( 'OutputString' %in% names( attributes(fmt.obj) ) ) {
    fmt.obj@OutputString = output.string;
  };

  if( !is.null( decimals) ) {
    fmt.obj@Decimals = decimals;
  };

  if( !is.null( as.percent) ) {
    fmt.obj@AsPercent = as.percent;
  };

  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.SharpeRatio
#
# Get format object for Sharpe ratio data
##########################################################################################################

get.format.SharpeRatio <- function( output.string = TRUE ) {
  fmt.obj = new( 'FormatValues', Decimals = 2, AsPercent = FALSE );
  fmt.obj@OutputString = output.string;
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.Return
#
# Get format object for return data
##########################################################################################################

get.format.Return <- function( decimals = 1, AsPercent = TRUE, output.string = TRUE ) {
  fmt.obj = new( 'FormatValues', Decimals = decimals, AsPercent = TRUE );
  fmt.obj@OutputString = output.string;
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.Price
#
# Get format object for price data
##########################################################################################################

get.format.Price <- function( output.string = TRUE ) {
  fmt.obj = new( 'FormatValues', Decimals = 2, AsPercent = FALSE );
  fmt.obj@OutputString = output.string;
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.Quantity
#
# Get format object for quantity data
##########################################################################################################

get.format.Quantity <- function( smallest.unit = 1, output.string = TRUE ) {
  if( smallest.unit > 1 ) {
    decimals = -log10( smallest.unit );
  } else {
    decimals = 0;
  };

  fmt.obj = new( 'FormatValues', Decimals = decimals, UseCommaToSeparateThousands = TRUE );
  fmt.obj@OutputString = output.string;
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.MarketValue
#
# Get format object for market value data
##########################################################################################################

get.format.MarketValue <- function( smallest.unit = 1, output.string = TRUE ) {
  fmt.obj = get.format.Quantity( smallest.unit = smallest.unit );
  fmt.obj@OutputString = output.string;
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.Generic
#
# Get the format object for generic data
##########################################################################################################

get.format.Generic <- function( decimals = 1, AsPercent = FALSE, output.string = TRUE ) {

  fmt.obj = new( 'FormatValues', Decimals = decimals, AsPercent = AsPercent );
  fmt.obj@OutputString = output.string;

  if( !AsPercent ) {
    fmt.obj@UseCommaToSeparateThousands = TRUE;
  } else {};

  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.MMDDYYYY
#
# Get format object for mm/dd/yyyy (e.g., 4/12/1979)
##########################################################################################################

get.format.MMDDYYYY <- function( separator = '/' ) {
  fmt.str = paste( c( '%m', '%d', '%Y' ), collapse = separator );
  fmt.obj = new( 'FormatDates', DateFormat = fmt.str );
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.DDMMYYYY
#
# Get format object for dd/mm/yyyy (e.g., 12/4/1979)
##########################################################################################################

get.format.DDMMYYYY <- function( separator = '/' ) {
  fmt.str = paste( c( '%d', '%m', '%Y' ), collapse = separator );
  fmt.obj = new( 'FormatDates', DateFormat = fmt.str );
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.YYYYMMDD
#
# Get format object for YYYY-MM-DD (e.g., 2010-04-21)
##########################################################################################################

get.format.YYYYMMDD <- function( separator = '-' ) {
  fmt.str = paste( c( '%Y', '%m', '%d' ), collapse = separator );
  fmt.obj = new( 'FormatDates', DateFormat = fmt.str );
  return( fmt.obj );
};

##########################################################################################################
# Routine: get.format.MMMYY
#
# Get format object for MMM-YY (e.g. Jul-01)
##########################################################################################################

get.format.MMMYY <- function( separator = '-' ) {
  fmt.str = paste( c( '%b', '%y' ), collapse = separator );
  fmt.obj = new( 'FormatDates', DateFormat = fmt.str );
  return( fmt.obj );
};  

##########################################################################################################
# Routine: get.format.string
##########################################################################################################

get.format.string <- function() {
  fmt.obj = new( 'FormatString' );
  return( fmt.obj );
};

##########################################################################################################
# Routine: format.values
#
# Format an array of values using a specified formatting type
##########################################################################################################

format.values <- function( format.type, vals, decimals = NULL, as.percent = NULL, output.string = TRUE ) {
  fmt.obj = get.format( format.type, decimals = decimals, as.percent = as.percent );
  fmt.obj@OutputString = output.string;
  fmt.vals = Format( fmt.obj, vals );
  return( fmt.vals );
};

##########################################################################################################
# Routine: format.df
#
# Format a data frame
##########################################################################################################

format.df <- function( df, format.pars, by.cols = TRUE ) {
  if( by.cols ) {
    N = ncol( df );
    labels = colnames(df);
  } else {
    N = nrow( df );
    labels = rownames(df);
  };

  if( is.list( format.pars) ) {
    if( N != length(format.pars) ) {
      if( by.cols ) {
        stop( 'Input format.pars must have one element for each column of the data frame.' );
      } else {
        stop( 'Input format.pars must have one element for each row of the data frame.' );
      };
    } else {
      pars = format.pars;
    };
  } else if( is.hash(format.pars) ) {
    pars = list();
    K = keys(format.pars);

    for( i in 1:N ) {
      label = labels[i];
      if( label %in% K ) {
        pars[[i]] = format.pars[[ label ]];
      };
    };
  } else {
    stop( 'Unsupported data type for input format.pars.' );
  };

  for( k in 1:N ) { 
    # If 'pars' is a string, then get the Format object
    if( is.character( pars[[k]] ) ) {
      pars[[k]] = get.format( format.type = pars[[k]] );
    } else {};
  };

  formatted.df = df;
  for( j in 1:N ) {
    if( !is.empty( pars[[j]] ) ) {
      if( by.cols ) {
        # Format by column
        formatted.df[,j] = Format( pars[[j]], df[,j] );
      } else {
        # Format by row
        formatted.df[j,] = Format( pars[[j]], df[j,] );
      };
    } else {};
  };

  return( formatted.df );
};


