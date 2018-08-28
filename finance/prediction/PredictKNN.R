library( locfit );

setClass( "PredictKNN", 
  representation( 
     alpha = 'numeric', 
     weightName='character', 
     indicatorWeights = 'numeric', 
     polyOrder = 'numeric',
     robustIter = 'numeric'
   ),
  contains = 'PredictionBase'
);

setMethod( "initialize", "PredictKNN",
  function( .Object, indicators = matrix(), responses = matrix(), indicatorNames = '', responseNames = '',
		alpha = 1/3, weightName = 'tricube', polyOrder = 1, robustIter = 3, 
	        indicatorWeights = rep( 1/ncol( indicators ), ncol( indicators ) )	
           ) {

   # Call 'initialize' method from the base class
   .Object = callNextMethod( .Object, indicators = indicators, responses = responses, 
						indicatorNames = indicatorNames, responseNames = responseNames );
    .Object@alpha = alpha;
    .Object@polyOrder = polyOrder;
    .Object@indicatorWeights = indicatorWeights;
    .Object@robustIter = 3;
   
    return( .Object );
  }
);

setGeneric( "lf", def=function( .Object, maxk = 100 ) standardGeneric( "lf" ) );
setMethod(  "lf", "PredictKNN", 
  function( .Object, maxk = 100 ) {

    if( maxk > 300 ) {
      return( NA )
    };

    locf = try( locfit.robust( .Object@indicators, .Object@responses, deg = .Object@polyOrder, alpha = .Object@alpha, 
						scale = TRUE, maxk = maxk ), silent = T );
    if( class( locf ) == 'try-error' ) {
      locf = lf( .Object, maxk = maxk + 100 );
    };

    return( locf );
  }
);

setGeneric( "predict.PredictKNN", def=function( .Object, testIndicators = .Object@indicators ) standardGeneric( "predict.PredictKNN" ) );
setMethod(  "predict.PredictKNN", "PredictKNN", 
  function( .Object, testIndicators = .Object@indicators ) {

    locf = lf( .Object );
    predictedValues = predict( locf, testIndicators );
    return( predictedValues )
  }
)


setGeneric( "selectBandwidth", def=function( .Object, stepSize = .01 ) standardGeneric( "selectBandwidth" ) );
setMethod(  "selectBandwidth", "PredictKNN",
  function( .Object, stepSize = .01 ) {
    alphas = seq( from = .01, to = 1, by = stepSize );
    cv = c();
    for( alpha in alphas ) {
      .Object@alpha = alpha;
      print( alpha );
      cv = c( cv, crossval( .Object ) ); 
    };

    return( cv )
  }
);

setGeneric( "aic.PredictKNN", def=function( .Object, stepSize = .01 ) standardGeneric( "aic.PredictKNN" ) );
setMethod(  "aic.PredictKNN", "PredictKNN",
  function( .Object, stepSize = .01 ) {
    alphas = seq( from = .01, to = 1, by = stepSize );
    AIC = c();
    for( alpha in alphas ) {
      aic.all = try( aic( .Object@indicators, .Object@responses, deg = .Object@polyOrder, scale = TRUE,alpha = alpha ), silent = TRUE );
      if( class( aic.all ) == 'try-error' ) {
        AIC = c( AIC, NaN );
      } else { 
        AIC = c( AIC, as.numeric( aic.all[ names( aic.all ) == 'aic' ] ) );
      };
    };

    return( AIC )
  }
);

setGeneric( "plot.conf", def=function( .Object, conf = 0.95, col = 'black', ylim = c() ) standardGeneric( "plot.conf" ) );
setMethod(  "plot.conf", "PredictionBase",
  function( .Object, conf = 0.95, col = 'black', ylim = c() ) {
    locf = lf( .Object );
    crit( locf ) = crit( locf, cov = conf );

    if( identical( ylim, c() ) ) {
      ylim = c( min( .Object@responses ), max( .Object@responses ) );
    };

    plot( locf, ylim = ylim, col = col, band='local' ); 
  }
);


