#!/bin/bash

# Define the path to R libraries for updating the database
if [ $HOME = /root ] 
then
  # Linode server
  DATABASE_PATH=/home/chris/programming/R/database
  DATA_PATH=/home/chris/programming/R/data
  MYSQL_DB_NAME=Finance
else
  # Mac
  DATABASE_PATH=$HOME/programming/R/database
  DATA_PATH=$HOME/programming/R/data
  MYSQL_DB_NAME=Finance
fi

echo "Updating data in table 'EquityData'..."
R --slave --file=$DATABASE_PATH/yahoo/update_EquityData.R --args $DATA_PATH $MYSQL_DB_NAME

echo 'Updating dividends...'
R --slave --file=$DATABASE_PATH/yahoo/update_dividends.R --args $DATA_PATH

echo 'Updating stock splits...'
R --slave --file=$DATABASE_PATH/yahoo/update_StockSplits.R

echo 'updating currency info...'
R --slave --file=$DATABASE_PATH/investing/update_currencies.R 

echo 'updating index constituents...'
R --slave --file=$DATABASE_PATH/indices/update_indices.R

#echo 'Updating Betas...'
# update.beta();

echo 'Updating Fred data...'
R --slave --file=$DATABASE_PATH/fred/update_FREDData.R --args $DATA_PATH $MYSQL_DB_NAME

echo 'Updating VIX Futures...'
R --slave --file=$DATABASE_PATH/VIX/update_VIX.R --args $DATA_PATH

# echo "Updating data in table 'YahooOptions'..."
# bash $DATABASE_PATH/yahoo/update_YahooOptions.sh $DATA_PATH $DATABASE_PATH $MYSQL_DB_NAME

# echo "Updating ETF proxies..."
# save.etf.proxies();

# echo "Updating Option proxies..."
# save.option.proxies();

# echo 'Updating ETF Alpha...'
# save.etf.alphas();

# Update list of ETF's and single stocks, and then download fundamental data
echo 'Updating ETF List...'
R --slave --file=$DATABASE_PATH/yahoo/update_ETFList.R --args $DATA_PATH

echo 'Updating Stock List...'
R --slave --file=$DATABASE_PATH/yahoo/update_StockList.R --args $DATA_PATH

echo 'Updating Equity Data...'
R --slave --file=$DATABASE_PATH/yahoo/update_CompanyData.R --args $DATA_PATH $MYSQL_DB_NAME


