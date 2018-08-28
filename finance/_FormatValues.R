
setClass(  
   "FormatValues", 
  representation( 
      Decimals = 'numeric',
      UseCommaToSeparateThousands = 'logical',
      AsPercent = 'logical', 
      UsePercentSymbol = 'logical',
      EmptySymbol = 'character',
      OutputString = 'logical'
   ), 
);

setMethod( "initialize", "FormatValues",
   function( .Object, Decimals = 2, AsPercent = FALSE,
		UseCommaToSeparateThousands = FALSE, 
		EmptySymbol = '' ) {

     .Object@Decimals = Decimals;
     .Object@UseCommaToSeparateThousands = UseCommaToSeparateThousands;
     .Object@EmptySymbol = EmptySymbol;

     # By default, have the object output a formatted string
     .Object@OutputString = TRUE;

     .Object@AsPercent = AsPercent;
     if( AsPercent ) { 
       .Object@UsePercentSymbol = TRUE;
     } else {
       .Object@UsePercentSymbol = FALSE;
     };

     return( .Object );
   }
);

setGeneric( "Decimals", def=function( .Object ) standardGeneric( "Decimals" ) );
setMethod(  "Decimals", "FormatValues", function( .Object ) return( .Object@Decimals ) );

setGeneric( "AsPercent", def=function( .Object ) standardGeneric( "AsPercent" ) );
setMethod(  "AsPercent", "FormatValues", function( .Object ) return( .Object@AsPercent ) );

setMethod(  "Format", "FormatValues", 
  function( .Object, inputs ) {

    if( .Object@AsPercent ) {
      inputs = inputs * 100;
    } else {};

    mult = 10^.Object@Decimals;
    dec.vals = round( inputs * mult ) / mult;

    if( .Object@OutputString ) {
      if( .Object@UseCommaToSeparateThousands ) {
        fmt.vals = formatC( dec.vals, format = 'f', digits = .Object@Decimals, big.mark = ',' );
      } else {
        fmt.vals = formatC( dec.vals, format = 'f', digits = .Object@Decimals );
      };
    
      if( .Object@UsePercentSymbol ) {
        fmt.vals = paste( fmt.vals, '%', sep = '' );
      };
    
      # Replace the empty input values with the 'EmptySymbol' string
      nan.idx = is.na(inputs) | is.nan( inputs );
      fmt.vals[ nan.idx ] = .Object@EmptySymbol;
    } else {
      fmt.vals = dec.vals;
    };
    
    return( fmt.vals );
  }
);

