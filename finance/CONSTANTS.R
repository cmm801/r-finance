HOSTNAME = system( "hostname", intern = TRUE );
HOME     = system( "echo $HOME", intern = TRUE );
if( HOME == "/root" )
  HOME = "/home/chris";

# URL for my website
WEBSITE_URL = '/home/chris/public/chrismillergallery.com/public/investing';

# Location of R scripts
BASE_PATH     = paste( HOME, '/programming/R',          sep = "" );
FINANCE_PATH  = paste( HOME, '/programming/R/finance',  sep = "" );
DATA_PATH     = paste( HOME, '/programming/R/data',     sep = "" );
DATABASE_PATH = paste( HOME, '/programming/R/database', sep = "" );
SQL_PATH      = paste( HOME, '/programming/SQL',        sep = "" );

# Location of external data files (calculated)
US_OPTION_EXPIRIES_TXT = sprintf( "%s/calculated/US_Option_expiries.txt", DATA_PATH );
VIX_EXPIRIES_TXT       = sprintf( "%s/calculated/VIX_expiries.txt", DATA_PATH );
ASSET_BETAS_CSV        = sprintf( "%s/calculated/asset_betas.csv", DATA_PATH );
ETF_ALPHA_CSV          = sprintf( "%s/calculated/etf_alpha.csv", DATA_PATH );
ETF_PROXIES_CSV        = sprintf( "%s/calculated/etf_proxies.csv", DATA_PATH );
OPTION_PROXIES_CSV     = sprintf( "%s/calculated/option_proxies.csv", DATA_PATH );

# Information about database
DB_STATUS_CSV          = sprintf( '%s/db_status.csv', DATA_PATH )

# Location of external data files (downloads)
VIX_CSV                = sprintf( "%s/downloads/VIX.csv", DATA_PATH );
SCHILLER_CSV           = sprintf( "%s/downloads/SchillerData.csv", DATA_PATH );
DIVIDENDS_CSV          = sprintf( "%s/downloads/dividends.csv", DATA_PATH );
FF_FACTORS_CSV         = sprintf( "%s/downloads/fama_french_factors.csv", DATA_PATH );
INDEX_CONSTITUENTS_TXT = sprintf( "%s/downloads/index_constituents.txt", DATA_PATH );
STOCK_SPLIT_CSV        = sprintf( "%s/downloads/stock_splits.csv", DATA_PATH ); 
EQUITY_CCY_DENOM_TXT   = sprintf( "%s/downloads/equity_currency_denominations.txt", DATA_PATH );
EXCHANGE_RATES_RECENT_CSV = sprintf( "%s/downloads/exchange_rate_info.csv", DATA_PATH );
FORWARD_RATES_RECENT_CSV  = sprintf( "%s/downloads/forward_rate_info.csv", DATA_PATH );
ETF_LIST_CSV              = sprintf( "%s/downloads/etf_list.csv", DATA_PATH );
EQUITY_EXCHANGE_INFO_CSV  = sprintf( "%s/downloads/stock_exchanges/exchange_info.csv", DATA_PATH ); 
EQUITY_EXCHANGE_LISTS_DETAILED = sprintf( "%s/downloads/stock_exchanges/exchange_detailed_listings", DATA_PATH );
EQUITY_EXCHANGE_LISTS_SIMPLE   = sprintf( "%s/downloads/stock_exchanges/exchange_simple_listings", DATA_PATH ); 

# Location of external data files (inputs)
ETF_INFORMATION_CSV    = sprintf( "%s/inputs/etf_information.csv", DATA_PATH );
TRANSACTIONS_CSV       = sprintf( "%s/inputs/transactions.csv", DATA_PATH );
INDEX_LIST_TXT         = sprintf( "%s/inputs/index_list.txt", DATA_PATH );
EXCHANGE_RATE_INFO_CSV = sprintf( "%s/inputs/exchange_rate_info.csv", DATA_PATH );
TABLE_DATE_INFO_CSV    = sprintf( "%s/inputs/table_date_info.csv", DATA_PATH );
ASSET_CLASSIFICATION_CSV = sprintf( "%s/inputs/asset_classification.csv", DATA_PATH );

# Location of equity and option symbols lists, which determine which data to download
OPT_LIST_INPUTS              = sprintf( "%s/inputs/symbol_lists/option_symbol_list.txt", DATA_PATH );
EQTY_LIST_INPUTS             = sprintf( "%s/inputs/symbol_lists/equity_symbol_list.txt", DATA_PATH );
INTRADAY_OPT_LIST_INPUTS     = sprintf( "%s/inputs/symbol_lists/intraday_option_symbol_list.txt", DATA_PATH );
INTRADAY_EQTY_LIST_INPUTS    = sprintf( "%s/inputs/symbol_lists/intraday_equity_symbol_list.txt", DATA_PATH );
OPT_LIST_CALCULATED           = sprintf( "%s/calculated/symbol_lists/option_symbol_list.txt", DATA_PATH );
EQTY_LIST_CALCULATED          = sprintf( "%s/calculated/symbol_lists/equity_symbol_lists/equity_symbol_list.txt", DATA_PATH );
INTRADAY_OPT_LIST_CALCULATED  = sprintf( "%s/calculated/symbol_lists/intraday_option_symbol_list.txt", DATA_PATH );
INTRADAY_EQTY_LIST_CALCULATED = sprintf( "%s/calculated/symbol_lists/intraday_equity_symbol_list.txt", DATA_PATH );

if( !exists( 'MARKOV_BLOCK_SIZE' ) )
  MARKOV_BLOCK_SIZE = 1;

# If using the Mac Book, we need to use a different mysql socket
if( HOSTNAME == 'madagascar' ) {
  MYSQL_SOCKET = "";
} else {
  MYSQL_SOCKET = "/opt/local/var/run/mysql5/mysqld.sock";
};

MYSQL_PASSWORD = 'Freeber1';
MYSQL_DB_NAME = 'Finance';



