
source( sprintf( "%s/finance/LIBRARY.R", BASE_PATH ) );
output.pathname = paste( WEBSITE_URL, 'portfolio', sep = '/' );
asOfDate = last.biz.date();
print.pf.summary( asOfDate = asOfDate, output.pathname = output.pathname );

