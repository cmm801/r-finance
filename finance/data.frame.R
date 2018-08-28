
###########################################################################################
# Routine: add.df.rows
###########################################################################################

add.df.rows <- function( df, labels, fill.value = NA ) {
  new.row.names = setdiff( labels, rownames( df ) );
  new.data = data.frame( matrix( fill.value, ncol = ncol(df), nrow = length(new.row.names) ) );
  rownames(new.data) = new.row.names;
  colnames(new.data) = colnames(df);;
  new.df = rbind( df, new.data );
  return( new.df );
};

###########################################################################################
# Routine: add.df.columns
###########################################################################################

add.df.columns <- function( df, labels, fill.value = NA ) {
  new.col.names = setdiff( labels, colnames( df ) );
  new.data = data.frame( matrix( fill.value, nrow = nrow(df), ncol = length(new.col.names) ) );
  colnames(new.data) = new.col.names;
  rownames(new.data) = rownames(df);;
  new.df = cbind( df, new.data );
  return( new.df );
};

###########################################################################################
# Routine: remove.df.rows
###########################################################################################

remove.df.rows <- function( df, labels ) {
  new.df = df[ !( rownames(df) %in% labels ), ];
  return( new.df );
};

###########################################################################################
# Routine: remove.df.columns
###########################################################################################

remove.df.columns <- function( df, labels ) {
  new.df = df[, !( colnames(df) %in% labels ), ];
  return( new.df );
};

###########################################################################################
# Routine: aggregate.df
###########################################################################################

aggregate.df <- function( 
  df, 
  by.cols, 
  agg.methods, 
  cols.to.keep = NULL
) {

  if( !is.null( cols.to.keep ) ) {
    # Only keep target cols
    agg.cols = cols.to.keep;
  } else {
    # Keep all columns not in 'by.cols'
    agg.cols = setdiff( colnames(df), by.cols );
  };

  # Find the key for each row, constructed from the 'by.cols'
  by.df = df[ by.cols ];
  new.df =  as.data.frame( by.df[ !duplicated( by.df ), ] );
  colnames( new.df ) = by.cols;

  # Construct an empty data frame for the numeric data
  numeric.df = as.data.frame( matrix( NaN, nrow = nrow(new.df), ncol = length(agg.cols) ) );
  colnames( numeric.df ) = agg.cols;
  
  # Combine the aggregeted 'by-cols' with the empty numeric data frame
  new.df = cbind( new.df, numeric.df );

  # Find the keys to map to the new data frame
  by.keys = c();
  for( i in 1:nrow(df) ) {
    by.keys[i] = paste( df[i,by.cols], collapse = '_' )
  };

  # Loop through the unique groups
  for( k in 1:nrow(new.df) ) {
    group.locs = by.keys == paste( new.df[k,by.cols], collapse = '_' );
  
    # Loop through the agg.cols, and aggregate each by the agg.method
    for( j in 1:length(agg.cols) ) {
      agg.col = agg.cols[j];

      if( length( agg.methods ) == 1 ) {
        agg.fun = agg.methods;
      } else {
        agg.fun = agg.methods[[ agg.col ]];
      };

      group.vals = df[ ,agg.col ][ group.locs ];
      new.df[k,agg.col] = agg.fun( group.vals );
    };
  };

  return( new.df );
};




