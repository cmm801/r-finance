
setClass(  
   "PredictRandomForest", 
  representation( 
     RF = 'randomForest'
   ),
  contains = 'PredictionBase'
);

setGeneric( "train.PredictRandomForest", def=function( .Object ) standardGeneric( "train.PredictRandomForest" ) );
setMethod(  "train", "PredictRandomForest", 
  function( .Object ) {
    .Object@RF = randomForest( .Object@indicators, .Object@responses );
    return( .Object );
  }
);

setGeneric( "predict.PredictRandomForest", def=function( .Object, testIndicators = .Object@indicators ) 
									standardGeneric( "predict.PredictRandomForest" ) );
setMethod(  "predict.PredictRandomForest", "PredictRandomForest", 
  function( .Object, testIndicators = .Object@indicators ) {
    predictedValue = predict( .Object@RF, testIndicators );
    return( predictedValue )
  }
)

