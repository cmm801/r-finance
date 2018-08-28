
setClass(  
   "PredictLinearRegression", 
  representation( 
     alpha = 'numeric', 
     beta  = 'numeric', 
     weights = 'numeric'
   ),
  contains = 'PredictionBase'
);

setMethod( "initialize", "PredictLinearRegression",
  function( .Object, indicators = matrix(), responses = matrix(), indicatorNames = '', responseNames = '', 
					weights = rep( 1/nrow( responses ), nrow( responses ) ) ) {

   # Call 'initialize' method from the base class
   .Object = callNextMethod( .Object, indicators = indicators, responses = responses,
                                                indicatorNames = indicatorNames, responseNames = responseNames );
   .Object@weights = weights;

    return( .Object );
  }
);

setGeneric( "train.PredictLinearRegression", def=function( .Object ) standardGeneric( "train.PredictLinearRegression" ) );
setMethod(  "train", "PredictLinearRegression", 
  function( .Object ) {
    reg =  as.numeric( lm( .Object@responses ~ .Object@indicators )$coef, weights = .Object@weights );
    .Object@alpha = reg[1];
print( reg[1] );
    beta = reg[-1];
    beta[ is.na( beta ) ] = 0;
    .Object@beta = beta;
print( beta );
    return( .Object );
  }
);

setGeneric( "predict.PredictLinearRegression", def=function( .Object, testIndicators = .Object@indicators ) 
									standardGeneric( "predict.PredictLinearRegression" ) );
setMethod(  "predict.PredictLinearRegression", "PredictLinearRegression", 
  function( .Object, testIndicators = .Object@indicators ) {
    alpha = .Object@alpha;
    beta  = .Object@beta;

    predictedValue = alpha + testIndicators %*% beta;
    return( predictedValue )
  }
)

