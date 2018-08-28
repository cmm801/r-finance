
setClass(  
   "PredictCluster", 
  representation( 
     nClusters = 'numeric', 
     centers = 'numeric' 
   ),
  contains = 'PredictionBase'
);

setMethod( "initialize", "PredictCluster",
  function( .Object, indicators = matrix(), responses = matrix(), indicatorNames = '', responseNames = '' ) {
   # Call 'initialize' method from the base class
   .Object = callNextMethod( .Object, indicators = indicators, responses = responses,
                                                indicatorNames = indicatorNames, responseNames = responseNames );
    return( .Object );
  }
);

setGeneric( "train.PredictCluster", def=function( .Object, nClusters ) standardGeneric( "train.PredictCluster" ) );
setMethod(  "train", "PredictCluster", 
  function( .Object, nClusters ) {
    
    return( .Object );
  }
);

setGeneric( "predict.PredictCluster", def=function( .Object, testIndicators = .Object@indicators ) 
									standardGeneric( "predict.PredictCluster" ) );
setMethod(  "predict.PredictCluster", "PredictCluster", 
  function( .Object, testIndicators = .Object@indicators ) {
    alpha = .Object@alpha;
    beta  = .Object@beta;

    predictedValue = alpha + testIndicators %*% beta;
    return( predictedValue )
  }
)

