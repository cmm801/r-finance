
source( sprintf( "%s/database.R", DATABASE_PATH ) );
output = t( get.log.details() );
output[ is.na( output )  ] = 0;

library( R2HTML );
html.filename = '/home/chris/public/chrismillergallery.com/public/investing/process_status.html';
HTML( output, file = html.filename, append = FALSE, align = 'center' );
HTML( sprintf( "<br><b>Last Updated:</b> %s", Sys.Date() ), file=html.filename );

