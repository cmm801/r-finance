
###########################################################################################
# Routine: get.test.parameters
# 
# Get any parameters needed to setup and run the tests
###########################################################################################

get.test.parameters <- function( test_number = NA ) {

  pars = list();

  pars[[1]] = list( StartDate = as.Date( '2011-11-21' ), EndDate = as.Date( '2012-12-31' ) );
  pars[[2]] = list( StartDate = as.Date( '2011-11-21' ), EndDate = as.Date( '2015-12-31' ) );

  return( pars );
};

