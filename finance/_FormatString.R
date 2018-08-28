
setClass(  
   "FormatString", 
    representation( 
      formatType = 'character'
    )  
  , 
);

setMethod( "initialize", "FormatString",
   function( .Object ) {
     return( .Object );
   }
);

setMethod(  "Format", "FormatString", 
  function( .Object, inputs ) {
    return( inputs );
  }
);

