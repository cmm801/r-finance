#!/bin/bash

DATA_PATH=$1
DATABASE_PATH=$2
MYSQL_DB_NAME=$3

TICKER_LIST_INPUTS=$DATA_PATH/inputs/symbol_lists/option_symbol_list.txt 
TICKER_LIST_CALCULATED=$DATA_PATH/calculated/symbol_lists/option_symbol_list.txt 
TICKER_LIST=`cat $TICKER_LIST_INPUTS $TICKER_LIST_CALCULATED | sort | uniq`

FILENAME=$DATABASE_PATH/yahoo/update_YahooOptions.R

for ticker in $TICKER_LIST
do
	echo $ticker
	R --slave --file=$FILENAME --args $ticker $MYSQL_DB_NAME > /dev/null
done





