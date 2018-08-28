
##############################################################################################################################
#
# prediction.date - The date for which we are trying to predict the response variable
#
# prediction.horizon - How many periods into the future we are trying to predict the response
#
# indicator.names - The names of the variables used for prediction
#
# response.name - The name of the response being predicted
#
# method - The method used for prediction.  Supported methods are kNN (nearest neighbors), Random Forest#
#
# Arguments - variables that are specific to a particular method and necessary for predicting the response
#
#
#
#
#
#
#
##############################################################################################################################

source( 'research/prediction.R' );

##############################################################################################################################
# Routine: get.prediction.data
##############################################################################################################################

fed.indicator.names = c( 'INDPRO', 'CPIAUCNS', 'PAYEMS' );

get.prediction.data <- function( 
  start.date = today() - years(100), 
  fed.indicator.names = c() 
) {

  # Get FRED data
  fed.indicators = get.fed.data( fed.indicator.names, start.date = start.date, rm.na = FALSE );

  # Get Fama French factors and US Equity TR from 1926 onwards
  FF_rtns = get.ff.data();
  FF = prod.ts( FF_rtns );

  responses = prod.ts( Market );
  colnames( responses ) = 'MKT';

  all.ts  = merge.ts( list( responses, indicators ), rm.na = FALSE );
  rtns <- ri( all.ts, n = prediction.horizon, rm.na = FALSE );

  data = list( indicators = rtns[ , indicator.names ], response = rtns[, 'MKT' ] );

  return( data );
};

# Define constants
indicator.names = c( 'INDPRO', 'CPIAUCNS', 'PAYEMS' );
response.name   = 'SPXTR';
#method = 'Loess';
#method = 'Loess';
method = 'LinearRegression';
bandwidth = 0.1;
kNN = 20;
buffer = 1;
weight.method = 'tricube';
prediction.horizon = 12;
training.frequency = 12;
start.date = as.Date( '1970-01-01' );
p.regress = 0;

# Get Data
#data = get.prediction.data( indicator.names, response.names, prediction.horizon = prediction.horizon );
#data = generate.linear.signal( start.date = start.date );
data = generate.mean.reverting.signal( mean.reversion.strength = .1, signal.to.noise = 4, means = 20, start.date = start.date )
indicators = data$indicators;
response   = data$response;

# Predict responses for multiple prediction dates
dates = as.Date( tail( rownames( indicators ), 400 ) )
dates = dates[ dates < today() - months( prediction.horizon ) ];
predicted = numeric( length( dates ) ) * NaN;
actual    = numeric( length( dates ) ) * NaN;

for( i in seq( from = 1, to = length( dates ), by = training.frequency ) ) { 
  prediction.date = dates[i];

  if( i %% 12 == 1 ) {
    print( prediction.date );
  };

  output = predict.response( indicators, response, prediction.date, method = method, prediction.horizon = prediction.horizon, 
	training.frequency = training.frequency, kNN = kNN, buffer = buffer, bandwidth = bandwidth, weight.method = weight.method, 
  		p.regress = p.regress  );

  output.rows = i:( i + length( output$Actual ) - 1 ) ;
  actual[ output.rows ]    = output$Actual;
  predicted[ output.rows ] = output$Predicted;
};


