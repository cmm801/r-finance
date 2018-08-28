
setClass(  
   "PredictionBase", 
  representation( 
      indicators = "matrix",
      responses  = "matrix",
      indicatorNames = "character", 
      responseNames  = "character"
   ), 
);

setMethod( "initialize", "PredictionBase",
  function( .Object, indicators = matrix(), responses = matrix(), indicatorNames = '', responseNames = '' ) {
   .Object@indicators <- indicators;
   .Object@responses  <- responses;

    if( !identical( '', indicatorNames ) ) {
      .Object@indicatorNames = indicatorNames;
    } else {
      .Object@indicatorNames = paste( 'Indicators_', 1:ncol( indicators ), sep = '' );
    };

    if( !identical( '', responseNames ) ) {
      .Object@responseNames = responseNames;
    } else {
      .Object@responseNames = paste( 'Responses_', 1:ncol( responses ), sep = '' );
    };

    return( .Object );
  }
);

setGeneric( "indicators", def=function( .Object ) standardGeneric( "indicators" ) );
setMethod(  "indicators", "PredictionBase", function( .Object ) .Object@indicators );

setGeneric( "responses", def=function( .Object ) standardGeneric( "responses" ) );
setMethod(  "responses", "PredictionBase", function( .Object ) .Object@responses );

setGeneric( "indicatorNames", def=function( .Object ) standardGeneric( "indicatorNames" ) );
setMethod(  "indicatorNames", "PredictionBase", function( .Object ) .Object@indicatorNames );

setGeneric( "responseNames", def=function( .Object ) standardGeneric( "responseNames" ) );
setMethod(  "responseNames", "PredictionBase", function( .Object ) .Object@responseNames );

setGeneric( "setParams", def=function( .Object, indicators = matrix(), responses = matrix() ) standardGeneric( 'setParams' ) );
setMethod(  "setParams", "PredictionBase",
  function( .Object, indicators = matrix(), responses = matrix() ) {
    if( !identical( indicators, matrix() ) ) {
      .Object@indicators = indicators;
    };

    if( !identical( responses, matrix() ) ) {
      .Object@responses = responses;
    };

    return( .Object );
  }
);

setGeneric( "train", def=function( .Object, ... ) standardGeneric( 'train' ) );
setMethod(  "train", "PredictionBase", 
  function( .Object, ... ) {
    return( .Object );
  }
);

setGeneric( "predict.PredictionBase", def=function( .Object, testIndicators = c() ) standardGeneric( "predict.PredictionBase" ) );
setMethod(  "predict.PredictionBase", "PredictionBase", 
  function( .Object, testIndicators = c() ) {
    return( NA );
  }
)

setGeneric( "crossval", def=function( .Object, nPartitions = 5, cv.type = 'block' ) standardGeneric( "crossval" ) );
setMethod(  "crossval", "PredictionBase", 
  function( .Object, nPartitions = 5, cv.type = 'block' ) {
    cv.info = crossval.res( .Object, nPartitions = nPartitions, cv.type = cv.type );    
    return( sd( cv.info$y - cv.info$x ) );
  }
);

setGeneric( "crossval.res", def=function( .Object, nPartitions = 5, cv.type = 'block' ) standardGeneric( "crossval.res" ) );
setMethod(  "crossval.res", "PredictionBase", 
  function( .Object, nPartitions = 5, cv.type = 'block' ) {
    PM = .Object;

    nObs = nrow( .Object@indicators );
    k = floor( nObs * ( 1 - 1/nPartitions) );

    predictedValues = c();
    actualValues = c();

    for( p in 1:nPartitions ) {
      if( cv.type == 'sample' ) {
        partition = sample( 1:nObs );
        trainingInds = partition[1:k];
        testInds = partition[(k+1):nObs];
      } else if( cv.type == 'block' ) {
        testInds = ceiling(1 + (p-1) * nObs / nPartitions ):floor( p*nObs / nPartitions );
        trainingInds = (1:nObs)[ -testInds ];
      } else {
        stop( paste( 'Unsupported cv.type: ', cv.type ) );
      };

      PM@indicators = matrix( .Object@indicators[trainingInds, ], ncol = ncol( .Object@indicators ) )
      PM@responses = matrix( .Object@responses[trainingInds ] );

      PM = train( PM );
      testIndicators = matrix( .Object@indicators[ testInds, ], ncol = ncol( .Object@indicators ) );
 
      new.pred  = try( predict( PM, testIndicators ), silent = TRUE );

      if( class( new.pred ) == 'try-error' ) {
        predictedValues = c( predictedValues, NaN * testIndicators ); 
      } else {
        predictedValues = c( predictedValues, predict( PM, testIndicators ) );
      };

      actualValues = c( actualValues, .Object@responses[ testInds, ] );
    };

    return( list( x = actualValues, y = predictedValues ) );
  }
)

setGeneric( "residuals.PredictionBase", def=function( .Object ) standardGeneric( "residuals.PredictionBase" ) );
setMethod(  "residuals.PredictionBase", "PredictionBase",
  function( .Object ) {
    res = predict( .Object, .Object@indicators ) - .Object@responses;
    return( res );
  }
);

setGeneric( "points.PredictionBase", def=function( .Object, col = 'black' ) standardGeneric( "points.PredictionBase" ) );
setMethod(  "points.PredictionBase", "PredictionBase",
  function( .Object, col = 'black' ) {
    points( .Object@indicators, .Object@responses, col = col );
  }
);


setGeneric( "plot.pred", def=function( .Object, col = 'black' ) standardGeneric( "plot.pred" ) );
setMethod(  "plot.pred", "PredictionBase",
  function( .Object, col = 'black' ) {
    plot( sort( .Object@indicators ), predict( .Object, matrix( sort( .Object@indicators ) ) ), type = 'l', col = col );
  }
);

setGeneric( "lines.pred", def=function( .Object, col = 'black' ) standardGeneric( "lines.pred" ) );
setMethod(  "lines.pred", "PredictionBase",
  function( .Object, col = 'black' ) {
    lines( sort( .Object@indicators ), predict( .Object, matrix( sort( .Object@indicators ) ) ), type = 'l', col = col );
  }
);

setGeneric( "plot.PredictionBase", def=function( .Object, col = 'black' ) standardGeneric( "plot.PredictionBase" ) );
setMethod(  "plot.PredictionBase", "PredictionBase",
  function( .Object, col = 'black' ) {
    plot( .Object@indicators, .Object@responses, col = col );
  }
);


setGeneric( "lines.PredictionBase", def=function( .Object, col = 'black' ) standardGeneric( "lines.PredictionBase" ) );
setMethod(  "lines.PredictionBase", "PredictionBase",
  function( .Object, col = 'black' ) {
    lines( .Object@indicators, .Object@responses, col = col );
  }
);

setGeneric( "points.res", def=function( .Object, col = 'black', xtime = FALSE ) standardGeneric( "points.res" ) );
setMethod(  "points.res", "PredictionBase",
  function( .Object, col = 'black', xtime = FALSE ) {
    res = residuals( .Object );
    if( xtime ) {
      points( res, col = col );
    } else {
      points( .Object@indicators, res, col = col );
    };
  }
);

setGeneric( "plot.res", def=function( .Object, col = 'black', xtime = FALSE ) standardGeneric( "plot.res" ) );
setMethod(  "plot.res", "PredictionBase",
  function( .Object, col = 'black', xtime = FALSE ) {
    res = residuals( .Object );

    inds = .Object@indicators;
    knnObject = new( 'PredictKNN', indicators = inds, responses = matrix( res ) );

    knnObject@alpha = 1/3;
    knnObject@polyOrder = 1;

    knnObject = train( knnObject );
    fittedResiduals = predict( knnObject, knnObject@indicators );

    if( xtime ) {
      std = sd( fittedResiduals );
      plot( fittedResiduals, col = col, type = 'l', ylim = c( -2*std, 2*std ) );
    } else {
      X = knnObject@indicators;
      plot( X[ order(X) ], fittedResiduals[ order(X) ], col = col, type = 'l', ylim = c( min(res), max(res) ) );
    };
  }
);

