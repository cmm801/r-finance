
source( paste( FINANCE_PATH, "/_Simulation.R", sep = "" ) );

setClass("SimulationTS",representation( TS = 'timeSeries', nDays = 'numeric', ValidationMethod = 'character' ), contains="Simulation")

setMethod( "initialize", "SimulationTS",
   function( 	.Object, 
		TS = timeSeries(), 
		nDays = 0, 
		numSims = 1e4, 
		Name = '', 
		labelName = '', 
		labelType = 'Std', 
		dataName = '', 
		Type = '', 
		Params = list(), 
		Training = data.frame(), 
		Test = data.frame(), 
		PartitionMethod = 'FirstLast', 
		PartitionTrainPct = 1, 
 		ValidationMethod  = 'KS'
  ) {
     .Object@TS        <- TS;
     .Object@nDays     <- nDays;
     .Object@numSims   <- numSims;

     .Object@Name      <- Name;
     .Object@labelName <- ifelse( labelName == '', 'Label', labelName );
     .Object@labelType <- labelType;
     .Object@dataName  <- ifelse( dataName == '',  names( TS ), dataName  );

     .Object@Type     <- Type;

     .Object@Training <- Training;
     .Object@Test     <- Test;

     .Object@Params   <- Params;

     if( labelType( .Object ) == 'Returns' ) {
       Labels = returns( TS, nday.rtn  = nDays( .Object ) );
     } else if( labelType( .Object ) == 'Std' ) {
       Labels = rolling.sd( TS, nDays( .Object ) );
     } else {
       stop( paste( 'Unsupported labelType', labelType( .Object ) ) );
     };
   
     data = data.frame( TS[ 1:nrow( Labels ), ], Label = Labels );
     colnames( data ) = c( dataName( .Object ), labelName( .Object ) );

     .Object@Data <- data;

     .Object@ValidationMethod <- ValidationMethod;
     .Object@PartitionMethod   = PartitionMethod;
     .Object@PartitionTrainPct = PartitionTrainPct;

     if( identical( Training, data.frame() ) || identical( Test, data.frame() ) ) {
       .Object = partition( .Object, PartitionMethod = PartitionMethod );
     };

     return( .Object );
   }
);

setGeneric( "TS", def=function( .Object ) standardGeneric( "TS" ) );
setMethod(  "TS", "SimulationTS", function( .Object ) .Object@TS );

setGeneric( "nDays",   def=function( .Object ) standardGeneric( "nDays" ) );
setMethod(  "nDays",   "SimulationTS", function( .Object ) .Object@nDays );

setGeneric( "ValidationMethod",   def=function( .Object ) standardGeneric( "ValidationMethod" ) );
setMethod(  "ValidationMethod",   "SimulationTS", function( .Object ) .Object@ValidationMethod );

setGeneric( "train", def=function( .Object ) standardGeneric( "train" ) );
setMethod(  "train", "SimulationTS",
   function( .Object ) {
      if( Type( .Object ) == 'Normal' ) {
         trainingData   <- Training( .Object )[ , labelName( .Object ) ];
	 .Object@Params <- list( Mean = mean( trainingData ) / nDays( .Object ), Std = sd( trainingData ) / sqrt( nDays( .Object ) ) );
      };

      return( .Object )
   }
);

setGeneric( "simulate", def=function( .Object, nsim = NA, seed = NULL ) standardGeneric( "simulate" ) );
setMethod(  "simulate", "SimulationTS",
   function( .Object, nsim = NA, seed = NULL ) {
      num.sims = ifelse( is.na( nsim ), numSims( .Object ), nsim );

      if( Type( .Object ) == 'Normal' ) {
 	mu    = Params( .Object )$Mean * nDays( .Object );
 	sigma = Params( .Object )$Std  * sqrt( nDays( .Object ) );
        sims  = rnorm( numSims( .Object ), mu, sigma );
      } else {
        stop( paste( 'Unsupported Type: ', Type( .Object ) ) );
      };

      return( sims );
   }
);

setGeneric( "crossValidate", def=function( .Object, numPartitions = 100, numValidations = 10, ZScore = TRUE ) standardGeneric( "crossValidate" ) );
setMethod(  "crossValidate", "SimulationTS",
   function( .Object, numPartitions = 100, numValidations = 10, ZScore = TRUE ) {
      validations = c();
      for( nP in 1:numPartitions ) {

        .Object = partition( .Object );
        .Object = train( .Object );
         
         sims = simulate( .Object );
     
         testLabels = Test( .Object )[, labelName( .Object ) ];

         if( ValidationMethod( .Object ) == 'KS' ) {
           # Kolmogorov-Smirnov Test
           ks.out = ks.test( testLabels, sims )
	   div = list( D = ks.out$statistic, p = ks.out$p ); 
         } else if( ValidationMethod( .Object ) == 'KL' ) {
	   # Kullback-Leibler Divergence
           div = kl.divergence( testLabels, sims )
         } else if( ValidationMethod( .Object ) == 'CumDiff' ) {
           div = calc.distrib.distance( true.dist = testLabels, theoretical.dist = sims, ZScore = FALSE );
         } else {
           stop( paste( 'Unsupported ValidationMethod: ', ValidationMethod ) );
         };

	 validations = c( validations, div );
      };

      if( ZScore == TRUE ) 
        validations = ZScore.distrib.distance( validations, true.dist = testLabels, theoretical.dist = sims );

      return( validations )
   }
);


