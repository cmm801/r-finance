
setClass(  
   "FormatDates", 
  representation( 
      DateFormat = 'character',
      EmptySymbol = 'character'
   ), 
);

setMethod( "initialize", "FormatDates",
   function( .Object, DateFormat = '%Y-%m-%d', EmptySymbol = '-' ) {
     .Object@DateFormat = DateFormat;
     .Object@EmptySymbol = EmptySymbol;
     return( .Object );
   }
);

setGeneric( "DateFormat", def=function( .Object ) standardGeneric( "DateFormat" ) );
setMethod(  "DateFormat", "FormatDates", function( .Object ) return( .Object@DateFormat ) );

setGeneric( "EmptySymbol", def=function( .Object ) standardGeneric( "EmptySymbol" ) );
setMethod(  "EmptySymbol", "FormatDates", function( .Object ) return( .Object@EmptySymbol ) );

setGeneric( "Format", def=function( .Object, inputs ) standardGeneric( "Format" ) );
setMethod(  "Format", "FormatDates", 
  function( .Object, inputs ) {

    fmt.dates = format( inputs, .Object@DateFormat );

    # Replace the empty input dates with the 'EmptySymbol' string
    nan.idx = is.na(inputs) | is.nan( inputs );
    fmt.dates[ nan.idx ] = .Object@EmptySymbol;
    
    return( fmt.dates );
  }
);

