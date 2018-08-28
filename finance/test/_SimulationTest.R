
setClass(  
   "SimulationTest", 
  representation( 
      Name       = "character", 
      dataName   = "character", 
      labelName  = "character", 
      labelType  = "character", 
      Type       = "character", 

      Data       = "data.frame",
      Training   = "data.frame",
      Test       = "data.frame",

      PartitionMethod  = "character", 
      PartitionTrainPct = 'numeric', 

      Params  = 'list', 
      numSims = 'numeric'
   ), 
);

setMethod( "initialize", "SimulationTest",
   function( .Object, Name = "", dataName = 'Data', labelName = 'Label', Type = "", Data = data.frame(), Params = list(), numSims = 1e4,
			PartitionMethod = '', PartitionTrainPct = 1, Training = data.frame(), Test = data.frame() ) {
     .Object@Name      <- Name;
     .Object@labelName <- labelName;
     .Object@dataName  <- dataName;

     .Object@Type     	     <- Type;

     .Object@PartitionMethod  <- PartitionMethod;
     .Object@PartitionTrainPct <- PartitionTrainPct;

     .Object@Data     <- Data;
     .Object@Training <- Training;
     .Object@Test     <- Test;

     .Object@Params  <- Params;
     .Object@numSims <- numSims;

      return( .Object );
    }
);

setGeneric( "Name",   def=function( .Object ) standardGeneric( "Name" ) );
setMethod(  "Name",   "SimulationTest", function( .Object ) .Object@Name );

setGeneric( "labelName",   def=function( .Object ) standardGeneric( "labelName" ) );
setMethod(  "labelName",   "SimulationTest", function( .Object ) .Object@labelName );

setGeneric( "labelType",   def=function( .Object ) standardGeneric( "labelType" ) );
setMethod(  "labelType",   "SimulationTest", function( .Object ) .Object@labelType );

setGeneric( "dataName",   def=function( .Object ) standardGeneric( "dataName" ) );
setMethod(  "dataName",   "SimulationTest", function( .Object ) .Object@dataName );

setGeneric( "Type",   def=function( .Object ) standardGeneric( "Type" ) );
setMethod(  "Type",   "SimulationTest", function( .Object ) .Object@Type );

setGeneric( "PartitionMethod",   def=function( .Object ) standardGeneric( "PartitionMethod" ) );
setMethod(  "PartitionMethod",   "SimulationTest", function( .Object ) .Object@PartitionMethod );

setGeneric( "PartitionTrainPct",   def=function( .Object ) standardGeneric( "PartitionTrainPct" ) );
setMethod(  "PartitionTrainPct",   "SimulationTest", function( .Object ) .Object@PartitionTrainPct );

setGeneric( "Data",   def=function( .Object ) standardGeneric( "Data" ) );
setMethod(  "Data",   "SimulationTest", function( .Object ) .Object@Data );

setGeneric( "Training",   def=function( .Object ) standardGeneric( "Training" ) );
setMethod(  "Training",   "SimulationTest", function( .Object ) .Object@Training );

setGeneric( "Test",   def=function( .Object ) standardGeneric( "Test" ) );
setMethod(  "Test",   "SimulationTest", function( .Object ) .Object@Test );

setGeneric( "Params",   def=function( .Object ) standardGeneric( "Params" ) );
setMethod(  "Params",   "SimulationTest", function( .Object ) .Object@Params );

setGeneric( "numSims",   def=function( .Object ) standardGeneric( "numSims" ) );
setMethod(  "numSims",   "SimulationTest", function( .Object ) .Object@numSims );

setGeneric( "train", def=function( .Object ) standardGeneric( "train" ) );
setMethod(  "train", "SimulationTest", 
   function( .Object ) {
      return( .Object )
   }
);   

setGeneric( "partition", def=function( .Object, PartitionMethod = '' ) standardGeneric( "partition" ) );
setMethod(  "partition", "SimulationTest", 
   function( .Object, PartitionMethod = '' ) {
      method = ifelse( PartitionMethod == '', PartitionMethod( .Object ), PartitionMethod );

      trainingSetSize = floor( nrow( Data( .Object ) ) * PartitionTrainPct( .Object ) );
      testSetSize     = nrow( Data( .Object ) ) - trainingSetSize;

      if( method == 'FirstLast' ) {
        locs = 1:trainingSetSize;
      } else if( method == 'Block' ) {
        startLoc = sample( trainingSetSize, 1 );
        locs     = c( 1:(startLoc-1) , ( startLoc + testSetSize ):nrow( Data( .Object ) ) );
      } else if( method == 'Random' ) {
        locs = sort( sample( 1:nrow( Data( .Object ) ), trainingSetSize ) );
      } else {
        stop( paste( 'Unsupported PartitionMethod: ', method ) )
      };
      
      .Object@Training = Data( .Object )[  locs, ];
      .Object@Test     = Data( .Object )[ -locs, ];

      return( .Object )
   }
);   

setGeneric( "simulate", def=function( .Object ) standardGeneric( "simulate" ) );
setMethod(  "simulate", "SimulationTest", 
   function( .Object ) {
      return( .Object )
   }
);   

setGeneric( "crossValidate", def=function( .Object, numPartitions = 100, numValidations = 10 ) standardGeneric( "crossValidate" ) );
setMethod(  "crossValidate", "SimulationTest", 
   function( .Object ) {
      return( .Object )
   }
);   

setGeneric( "test", def=function( .Object ) standardGeneric( "test" ) );
setMethod(  "test", "SimulationTest", 
   function( .Object ) {
      return( .Object )
   }
);   




