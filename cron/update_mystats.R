
source( sprintf( "%s/finance/LIBRARY.R", BASE_PATH ) );
output.pathname = WEBSITE_URL;
asOfDate = last.biz.date();
my.pf.stats( asOfDate = asOfDate, output.pathname = output.pathname );

