# Libraries

source( paste( FINANCE_PATH, "/asset.R", sep = "" ) );
source( paste( FINANCE_PATH, "/currency.R", sep = "" ) );
source( paste( FINANCE_PATH, "/data.R", sep = "" ) );
source( paste( FINANCE_PATH, "/utils.R", sep = "" ) );
source( paste( FINANCE_PATH, "/risk.R", sep = "" ) );
source( paste( FINANCE_PATH, "/database.R", sep = "" ) );
source( paste( FINANCE_PATH, "/data.frame.R", sep = "" ) );
source( paste( FINANCE_PATH, "/date.R", sep = "" ) );
source( paste( FINANCE_PATH, "/etf.R", sep = "" ) );
source( paste( FINANCE_PATH, "/options.R", sep = "" ) );
source( paste( FINANCE_PATH, "/portfolio.R", sep = "" ) );
source( paste( FINANCE_PATH, "/time.series.R", sep = "" ) );
source( paste( FINANCE_PATH, "/factor_data.R", sep = "" ) );
source( paste( FINANCE_PATH, "/equity_data.R", sep = "" ) );
source( paste( FINANCE_PATH, "/econometrics.R", sep = "" ) );
source( paste( FINANCE_PATH, "/my.portfolio.R", sep = "" ) );
source( paste( FINANCE_PATH, "/my.pf.utils.R", sep = "" ) );
source( paste( FINANCE_PATH, "/futures.R", sep = "" ) );
source( paste( FINANCE_PATH, "/simulations.R", sep = "" ) );
source( paste( FINANCE_PATH, "/performance.R", sep = "" ) );
source( paste( FINANCE_PATH, "/volatility.R", sep = "" ) );
source( paste( FINANCE_PATH, "/expiry.R", sep = "" ) );
source( paste( FINANCE_PATH, "/factors.R", sep = "" ) );
source( paste( FINANCE_PATH, "/bonds.R", sep = "" ) );
source( paste( FINANCE_PATH, "/format.R", sep = "" ) );

# Load classes
source( paste( FINANCE_PATH, "/_Trade.R", sep = "" ) );
source( paste( FINANCE_PATH, "/_Simulation.R", sep = "" ) );
source( paste( FINANCE_PATH, "/_Portfolio.R", sep = "" ) );
source( paste( FINANCE_PATH, "/_FormatDates.R", sep = "" ) );
source( paste( FINANCE_PATH, "/_FormatValues.R", sep = "" ) );
source( paste( FINANCE_PATH, "/_FormatString.R", sep = "" ) );

# Included Packages
library( XML, quietly = TRUE );
library( timeSeries, quietly = TRUE );
library( quantmod, quietly = TRUE );
library( RMySQL, quietly = TRUE );
library( PerformanceAnalytics, quietly = TRUE );
library( lubridate, quietly = TRUE )
library( hash, quietly = TRUE )
#library( data.table );
#library( ggplot2 );

options(stringsAsFactors = FALSE);


