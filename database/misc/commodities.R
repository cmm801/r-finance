

get.london.metals.prices <- function( type = 'silver' ) {

  for( year in 1968:year( today() ) ) {
    url = sprintf( 'http://www.lbma.org.uk/pages/index.cfm?page_id=54&title=silver_fixings&show=%d&type=daily', year );
  };


};

