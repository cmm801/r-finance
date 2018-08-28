
source( paste( FINANCE_PATH, "/graphics.R", sep = "" ) );

###########################################################################################
# Routine: find.last.date.value
#
# Get the time series values on the dates provided in the argument
# If no value is available on a given date, then the previous value is returned (default), 
#   or the values are linearly interpolated.  This choice is set by the 'method' argument.
# The 'rule' argument decides whether to return NA if the desired date lies outside the range
#   for the time series.
###########################################################################################

find.last.date.value <- function( 
  ts, 
  dates, 
  rule = 2,  # If rule = 1, NA's are used for extrapolation.  rule = 2, the closest endpoint is used
  method = 'constant' # method = 'linear', 'constant'
) {
  ts.dates = as.numeric( as.Date( rownames( ts )));
  dt = as.numeric( as.Date( dates ) );
 
  vals = matrix( NaN, ncol = ncol( ts ), nrow = length( dates ) );
  for( c in 1:ncol(ts) ) {
    vals[,c] = approx( ts.dates, as.numeric( ts[,c] ), dt, yleft = 0, rule = rule, method = method )$y;
  }

  if( nrow(vals) == 1 || ncol(vals) == 1 ) {
    vals = as.numeric(vals);
  };

  names( vals ) = colnames( ts );

  return( vals );
};

###########################################################################################
# Routine: last.date
###########################################################################################

last.date <- function( ts ) {
  ld = as.Date( rownames( tail( ts, 1 ) ) );
  return( ld );
};

###########################################################################################
# Routine: first.date
###########################################################################################

first.date <- function( ts ) {
  fd = as.Date( rownames( head( ts, 1 ) ) );
  return( fd );
};

###########################################################################################
# Routine: mdd.ts
###########################################################################################

mdd.ts <- function( ts, N = NA ) {
  if( is.na(N) ) {
    mdd = 1 - ts / rolling.max( ts );
  } else {
    mdd = 1 - ts / rolling.max( ts, N );
  };

  return(mdd);
};


###########################################################################################
# Routine: nanmerge
#
# Merge two time series and remove NA's
###########################################################################################

nanmerge <- function( ts1, ts2, rm.na = TRUE ) {
  if( is.empty(ts1) ) {
    return( ts2 );
  } else if( is.empty( ts2) ) {
    return( ts1 );
  } else {
    ts = merge.ts( list( ts1, ts2 ), rm.na = rm.na ); 
    return( ts );
  };
};

###########################################################################################
# Routine: daily2monthly.rtns
#
# Convert daily rtns to monthly
###########################################################################################

daily2monthly.rtns <- function( rtns, rm.na = FALSE ) {
  monthly.rtns = adjust.rtn.freq( rtns, freq = 'M', rm.na = rm.na );
  return( monthly.rtns );
};

###########################################################################################
# Routine: daily2monthly
#
# Convert daily time series to monthly
###########################################################################################

daily2monthly <- function( daily.ts ) {
  monthly.ts = adjust.ts.freq( daily.ts, 'M' );
  return( monthly.ts );
};

###########################################################################################
# Routine: adjust.ts.freq
#
# Adjust the frequency of a time series to yearly, monthly, or weekly (Y, M, W )
###########################################################################################

adjust.ts.freq <- function( ts, freq = 'M' ) {
  if( freq == 'D' ) {
    return(ts);
  };

  filled.ts = fill.missing( ts )

  start.date = as.Date( rownames( filled.ts[1,] ) );
  end.date = last.date( filled.ts );

  period.ends = get.period.ends( start.date, end.date, freq = freq );
  period.ends = period.ends[ start.date <= period.ends & period.ends <= end.date ];

  new.ts = filled.ts[ period.ends, ];
  return( new.ts );
};

###########################################################################################
# Routine: adjust.rtn.freq 
#
# Adjust the frequency of a time series of returns
###########################################################################################

adjust.rtn.freq <- function( rtns, freq = 'M', rm.na = FALSE ) {
  period.ts = adjust.ts.freq( prod.ts( rtns ) );
  period.rtns = ri( period.ts );
  return( period.rtns );
};

###########################################################################################
# Routine: sample.ts
#
# Sample weekly, monthly, quarterly, or annual data from a daily time series
###########################################################################################

sample.ts <- function( ts, period = 'm' ) {
  filled.ts = fill.missing( ts );

  start.date = min( as.Date( rownames( filled.ts ) ) );
  end.date   = max( as.Date( rownames( filled.ts ) ) );

  if( period == 'w' ) {
    dates = seq( from = start.date, to = end.date, by = 'w' );
  } else if( period == 'm' ) {
    dates = get.month.ends( start.date, end.date );
  } else if( period == 'q' ) {
    month.ends = get.month.ends( start.date, end.date );
    dates = month.ends[  month( month.ends ) %% 3 == 0 ];
  } else if( period == 'y' ) {
    month.ends = get.month.ends( start.date, end.date );
    dates = month.ends[  month( month.ends ) %% 12 == 0 ];
  } else {
    stop( paste( 'Unsupported period:', period ) )
  };
 
  dates = dates[ start.date <= dates & dates <= end.date ];
  
  sts = filled.ts[ dates, ]; 
  return( sts );
};

###########################################################################################
# Routine: fill.by.proxy
#
# Use the values from the full.ts to fill the partial.ts when it has missing values
###########################################################################################

fill.by.proxy <- function( partial.ts, full.ts ) {
  if( is.empty( partial.ts ) || sum( !is.na( partial.ts ) ) == 0 ) {
    return( full.ts );
  };

  pts = merge.ts( list( partial.ts, full.ts ), rm.na = FALSE )[, 1 ];
  pts = pts[ wday( rownames( pts ) ) %in% 2:6, ];

  if( nrow( full.ts ) < 2 ) { 
    return( pts ) 
  } else {
    full.rtns = ri( full.ts );
  };

  if( is.na( pts[1] ) ) {
    first.pt = min( which( !is.na( pts ) ) );
    for( i in rev( 1:(first.pt-1) ) ) {
      pts[i] = pts[i+1] / ( 1 + full.rtns[i] );
    }; 
  };

  for( i in 2:nrow( pts ) ) {
    if( is.na( pts[i] ) ) {
       pts[i] = pts[i-1] * ( 1 + full.rtns[i-1] );
    };
  };

  return( pts );
};

###########################################################################################
# Routine: calc.key.stats 
#
# outputs a list of relevant information about the portfolo time series
###########################################################################################

calc.key.stats <- function( 
  levels, 
  periods = NA, 
  asOfDate = last.biz.date()
) {
  # Remove NA's
  levels <- timeSeries( levels[ !is.na( levels ) ], rownames( levels[ !is.na( levels ), ] ), units = colnames( levels ) );

  if( is.na( periods ) ) {
    periods = c( '1b', '1w', '2w', '1m', '3m', '6m', '1y', '2y', 'WTD', 'MTD', 'YTD', 'ITD' );
    periods = c( periods, as.character( unique( year( rownames( levels ) ) ) ) );
  };

  # Define the start and end dates
  start.date  = first.date( levels ); 
  end.date    = asOfDate;

  df = data.frame( Start.Date = start.date, End.Date = last.date( levels ), nYears = number.of.years( levels ) );
  days.per.year = round( nrow( levels ) / df$nYears );
 
  if( nrow( levels ) < 2 ) {
    return( df );
  };

  for( Rdt in periods ) {
    sfx = ifelse( Rdt == '', '', sprintf( ".%s", Rdt ) );  

    if( Rdt %in% as.character(1800:2200) ) {
      tmp = levels[ year( row.names( levels ) ) == as.numeric( Rdt ), ];
    } else { 
      if( Rdt == 'WTD' ) {
        fridays = find.all.period.ends( start.date, end.date, period = 'week' );
        cutoff.date = max( fridays[ end.date - weeks(7) < fridays & fridays < end.date - 2 ] );
      } else if( Rdt == 'MTD' ) {
        month.ends  = find.all.period.ends( start.date %m-% months(1), end.date, period = 'month' );
        cutoff.date = max( month.ends[ month.ends %m-% months(1) < month.ends & month.ends < end.date - 2 ] );
      } else if( Rdt == 'YTD' ) {
        year.ends   = find.all.period.ends( start.date %m-% years(1), end.date, period = 'year' );
        cutoff.date = max( year.ends[ year.ends %m-% years(1) < year.ends & year.ends < end.date - 2 ] );
      } else if( Rdt == 'ITD' ) {
        cutoff.date = start.date
      } else {
        cutoff.date = DateRuleApply( end.date, sprintf( "-%s", Rdt ) );
      };

      tmp = levels[ as.Date( rownames( levels ) ) >= cutoff.date, ];
    };

    Rtn = ifelse( !is.empty(tmp), -1 + tail( as.numeric( tmp ) / as.numeric( tmp[1] ), 1 ), NA );
    Chg = ifelse( !is.empty(tmp), as.numeric( tail( tmp, 1 ) ) - as.numeric( head( tmp, 1 ) ), NA );
    Std = as.numeric( stddev.ts( tmp ) );
    SR = SharpeRatio.ts( tmp );

    new.df = data.frame( Rtn = Rtn, Std = Std, SR = SR, Chg = Chg );
    names( new.df ) = sapply( names( new.df ), function(x) sprintf( "%s%s", x, sfx ) );
    df = cbind( df, new.df )
  };

  # Calculate annualized return
  df$Cum.Ann.Rtn <- ( tail( levels, 1 ) )^(1/df$nYears) - 1;

  # Calculate downside/upside standard deviation
  if( !any( levels < 0 ) ) {
    rtns = ri( levels );
    rtns = rtns[ !is.nan(rtns), ];

    # Find Downside vol
    if( sum( rtns < mean( rtns ) ) > 1 ) {
      df$Downside.Std = sd( rtns[ rtns < mean( rtns ) ] ) * sqrt( days.per.year );
    } else {
      df$Downside.Std = NaN;
    };

    # Find upside vol
    if( sum( rtns > mean( rtns ) ) > 1 ) {
      df$Upside.Std = sd( rtns[ rtns > mean( rtns ) ] ) * sqrt( days.per.year );
    } else { 
      df$Upside.Std = NaN;
    };

    # Get Sortino Ratio
    df$Sortino.Ratio = as.numeric( df$Cum.Ann.Rtn / sd( rtns[ rtns < mean( rtns ) ] ) / sqrt( days.per.year ) );

    # Get higher moments
    df$Skewness = skewness( rtns );
    df$Kurtosis = kurtosis( rtns );
    
    # Get winning percentage stats
    df$Pct.Days.Winning = sum( rtns > 0 ) / length( levels );
    df$Max.1d.Win.Pct  = max( rtns );
    df$Max.1d.Loss.Pct = min( rtns );
  } else {
    df$Downside.Std = NaN;
    df$Upside.Std = NaN;
    df$Sortino.Ratio = NaN;
    df$Skewness = NaN;
    df$Kurtosis = NaN;
    df$Pct.Days.Winning = NaN;
    df$Max.1d.Win.Pct = NaN;
    df$Max.1d.Loss.Pct = NaN;
  };

  # Get Max Draw-down information 
  mdd = maxdrawdown.ts( levels );
  df$MaxDrawdown = mdd$maxdrawdown[1];
  df$MDDStart     = mdd$startDate[1];
  df$MDDEnd       = mdd$endDate[1];
  df$MDDRecovery  = mdd$recoveryDate[1];
  if( is.null( df$MDDRecovery ) ) {
    df$MDDRecovery = '-';
  };

  # Get Max 12m 'Draw-up' information 
  mdu = maxdrawup.ts( levels );
  df$MaxDrawup = mdu$maxdrawup[1]
  df$MDUStart  = mdu$startDate[1]
  df$MDUEnd    = mdu$endDate[1]
  df$MDURecovery = mdu$recoveryDate[1]
  if( is.null( df$MDURecovery ) ) {
    df$MDURecovery = '-';
  };

  # Get information about beginning value
  idx = max( which( levels == max( levels ) ) );
  dist = as.numeric( asOfDate ) - as.numeric( as.Date( rownames( levels[idx,] ) ) );
  df$PeakValueDate = as.Date( rownames( levels[idx,] ) );
  df$ReturnFromPeak =  -1 + last(levels) / levels[idx];
  df$TimeSincePeak = sprintf( '%s days', dist );
  
  return( df );
};

###########################################################################################
# Routine: get.ts.stats
###########################################################################################

get.ts.stats <- function( 
  all.ts,
  asOfDate = today()
) {
  for( i in 1:ncol( all.ts ) ) {
    ts <- ind.ts( all.ts[,i] );
    rtns <- ri( ts );

    start.date = as.Date( rownames( ts[1,] ) );
    Start.Date = format( start.date, '%m/%d/%y' );
    AsOfDate   = format( last.date( ts ), '%m/%d/%y' );
  
    years.since.inception <- number.of.years( ts );
    nYears <- sprintf( '%.2f', years.since.inception );

    year.ends = get.period.ends( start.date, last.date(ts), 'Y' );
    prev.year.end = year.ends[ max( which( year.ends < asOfDate ) ) ];

    ts.YTD  = ts.range( ts, start.date = prev.year.end );
    ts.2011 = ts.range( ts, start.date = as.Date( '2010-12-31' ), end.date = as.Date( '2011-12-31' ) );
    ts.2012 = ts.range( ts, start.date = as.Date( '2011-12-31' ), end.date = as.Date( '2012-12-31' ) );
    ts.2013 = ts.range( ts, start.date = as.Date( '2012-12-31' ), end.date = as.Date( '2013-12-31' ) );
    ts.2014 = ts.range( ts, start.date = as.Date( '2013-12-31' ), end.date = as.Date( '2014-12-31' ) );
    ts.2015 = ts.range( ts, start.date = as.Date( '2014-12-31' ), end.date = as.Date( '2015-12-31' ) );

    ts.1d   = tail( ts, 2 );
    ts.1w   = tail( ts, 6 );
    ts.2w   = tail( ts, 11 );
    ts.1m   = tail( ts, 22 );
    ts.3m   = tail( ts, 64 );
    ts.6m   = tail( ts, 127 );
    ts.12m  = tail( ts, 253 );

    Avg.Ann.Rtn <- sprintf( '%.1f', ( - 1 + tail( ts, 1 ) ) / years.since.inception * 100 );

    Std.Dev.YTD  <- sprintf( '%.1f', sd( ri( ts.YTD  ) ) * sqrt( 252 ) * 100 );
    Std.Dev.2011 <- sprintf( '%.1f', sd( ri( ts.2011 ) ) * sqrt( 252 ) * 100 );
    Std.Dev.2012 <- sprintf( '%.1f', sd( ri( ts.2012 ) ) * sqrt( 252 ) * 100 );
    Std.Dev.2013 <- sprintf( '%.1f', sd( ri( ts.2013 ) ) * sqrt( 252 ) * 100 );
    Std.Dev.2014 <- sprintf( '%.1f', sd( ri( ts.2014 ) ) * sqrt( 252 ) * 100 );
    Std.Dev.2015 <- sprintf( '%.1f', sd( ri( ts.2015 ) ) * sqrt( 252 ) * 100 );
    Std.Dev.ITD  <- sprintf( '%.1f', sd( ri( ts      ) ) * sqrt( 252 ) * 100 );
    Std.Dev.2w   <- sprintf( '%.1f', sd( ri( ts.2w   ) ) * sqrt( 252 ) * 100 );
    Std.Dev.1m   <- sprintf( '%.1f', sd( ri( ts.1m   ) ) * sqrt( 252 ) * 100 );
    Std.Dev.3m   <- sprintf( '%.1f', sd( ri( ts.3m   ) ) * sqrt( 252 ) * 100 );
    Std.Dev.6m   <- sprintf( '%.1f', sd( ri( ts.6m   ) ) * sqrt( 252 ) * 100 );
    Std.Dev.12m  <- sprintf( '%.1f', sd( ri( ts.12m  ) ) * sqrt( 252 ) * 100 );

    Rtn.YTD   <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.YTD  ), 1 ) ) * 100 );
    Rtn.2011  <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.2011 ), 1 ) ) * 100 );
    Rtn.2012  <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.2012 ), 1 ) ) * 100 );
    Rtn.2013  <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.2013 ), 1 ) ) * 100 );
    Rtn.2014  <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.2014 ), 1 ) ) * 100 );
    Rtn.2015  <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.2015 ), 1 ) ) * 100 );
    Rtn.ITD   <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts      ), 1 ) ) * 100 );
    Rtn.1d    <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.1d   ), 1 ) ) * 100 );
    Rtn.1w    <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.1w   ), 1 ) ) * 100 );
    Rtn.2w    <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.2w   ), 1 ) ) * 100 );
    Rtn.1m    <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.1m   ), 1 ) ) * 100 );
    Rtn.3m    <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.3m   ), 1 ) ) * 100 );
    Rtn.6m    <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.6m   ), 1 ) ) * 100 );
    Rtn.12m   <- sprintf( '%.1f', as.numeric( tail(-1 + ind.ts( ts.12m  ), 1 ) ) * 100 );

    Cum.Rtn.ITD <- sprintf( '%.1f', tail(ts,1)*100 - 100 );
    cum.rtn.float <- ( tail( ts, 1 ) )^(1/years.since.inception) - 1;
    Cum.Ann.Rtn   <- sprintf( "%.1f", cum.rtn.float * 100 );

    Downside.Std = sprintf( '%.1f', sd( rtns[ rtns < mean( rtns ) ] ) * sqrt( 252 ) * 100 )
    Upside.Std   = sprintf( '%.1f', sd( rtns[ rtns > mean( rtns ) ] ) * sqrt( 252 ) * 100 );

    # If not enough data points in year, then don't calculate Sharpe ratio
    if( nrow( ts.YTD ) > 2 ) {
      Sharpe.Ratio.YTD = sprintf( "%.2f", SharpeRatio.ts( ts.YTD ) );
    } else {
      Sharpe.Ratio.YTD = 'NA';
    };

    # Get Sharpe Ratios
    Sharpe.Ratio.ITD  = sprintf( "%.2f", SharpeRatio.ts( ts ) );
    Sharpe.Ratio.2011 = sprintf( "%.2f", SharpeRatio.ts( ts.2011 ) );
    Sharpe.Ratio.2012 = sprintf( "%.2f", SharpeRatio.ts( ts.2012 ) );
    Sharpe.Ratio.2013 = sprintf( "%.2f", SharpeRatio.ts( ts.2013 ) );
    Sharpe.Ratio.2014 = sprintf( "%.2f", SharpeRatio.ts( ts.2014 ) );
    Sharpe.Ratio.2015 = sprintf( "%.2f", SharpeRatio.ts( ts.2015 ) );

    # Get Sortino Ratio
    Sortino.Ratio = sprintf( '%.2f', cum.rtn.float / sd( rtns[ rtns < mean( rtns ) ] ) / sqrt( 252 ) );

    # Get Max Draw-down information 
    mdd = maxdrawdown.ts( ts ); 
    MaxDrawdown = sprintf( '%.1f', 100 * mdd$maxdrawdown );
    MDDStart     = mdd$startDate;
    MDDEnd       = mdd$endDate;
    MDDRecovery  = mdd$recoveryDate;
    if( is.null( MDDRecovery ) ) {
      MDDRecovery = '-';
    };

    # Get information about peak value
    idx = which( ts == max( ts ) );
    dist = as.numeric( asOfDate ) - as.numeric( as.Date( rownames( ts[idx,] ) ) );
    PeakValueDate = as.Date( rownames( ts[idx,] ) );
    ReturnFromPeak =  sprintf( '%.1f', 100 * ( -1 + last(ts) / ts[idx] ) );
    TimeSincePeak = sprintf( '%s days', dist );
    
    # Get higher moments
    Skewness = sprintf( "%0.1f", skewness( rtns ) );
    Kurtosis = sprintf( "%0.1f", kurtosis( rtns ) );

    Pct.Days.Winning = sprintf( '%.1f', 100 * sum( rtns > 0 ) / length( ts ) );

    Max.1d.Win.Pct  = sprintf( '%.1f', max( rtns )*100 );
    Max.1d.Loss.Pct = sprintf( '%.1f', min( rtns )*100 );

    new.stats = as.data.frame( t( data.frame(
                        Cum.Rtn.ITD, Avg.Ann.Rtn, Cum.Ann.Rtn,
                        Std.Dev.YTD, Std.Dev.2011, Std.Dev.2012, Std.Dev.2013, Std.Dev.2014, Std.Dev.2015, 
			Std.Dev.ITD, Std.Dev.2w, Std.Dev.1m, Std.Dev.3m, Std.Dev.6m, Std.Dev.12m, 
			Rtn.YTD, Rtn.2011, Rtn.2012, Rtn.2013, Rtn.2014, Rtn.2015, 
			Rtn.ITD, Rtn.1d, Rtn.1w, Rtn.2w, Rtn.1m, Rtn.3m, Rtn.6m, Rtn.12m, 
                        Downside.Std, Upside.Std, Sharpe.Ratio.YTD, Sharpe.Ratio.2011, 
			Sharpe.Ratio.2012, Sharpe.Ratio.2013, Sharpe.Ratio.2014, Sharpe.Ratio.2015, 

			Sharpe.Ratio.ITD, Sortino.Ratio,
                        MaxDrawdown, MDDStart, MDDEnd, MDDRecovery, 
			PeakValueDate, ReturnFromPeak, TimeSincePeak,
  			Skewness, Kurtosis, Pct.Days.Winning,
                        Max.1d.Win.Pct, Max.1d.Loss.Pct, 
                        Start.Date, AsOfDate, nYears ) ) );
    if( i == 1 ) {
      stats = new.stats;
    } else {
      stats = cbind( stats, new.stats );
    };
  };

  colnames( stats ) = colnames( all.ts );
  return(stats);
};

###########################################################################################
# Routine: maxdrawdown.ts
# 
# This is my fix to the 'maxdrawdown' function, which originally looks at the absolute
#   draw-down of a series, rather than the relative (percent) draw-down that one usually
#   associates with the Max-Draw-Down in finance.
###########################################################################################

maxdrawdown.ts <- function( ts, N = NA ) {
  if( any( as.numeric(ts) < 0 ) ) {
    output = list();
    output$maxdrawdown = NA;
    output$startDate = '';
    output$endDate = '';
    output$recoveryDate = '';
    output$beginning = NA;
    output$peak = NA;
    output$recovery = NA;
    return(output);
  };

  if( !is.na(N) ) {
    ts.max.expand = rolling.max( ts );
    ts.max = rolling.max( ts, N );
    ts.max[ is.na(ts.max), ] = ts.max.expand[ is.na(ts.max), ]; 
  } else {
    ts.max = rolling.max( ts );
  };

  ts.dd = 1 - ts / ts.max;
  max.dd = max( ts.dd );
  trough = which( max.dd == ts.dd );
  if( 0 == length( trough ) ) {
    output = list();
    output$maxdrawdown = NaN;
    output$startDate = '';
    output$endDate = '';
    output$recoveryDate = '';
    output$peak = NaN;
    output$trough = NaN;
    output$recovery = NaN;
    return( output );
  } else {
    trough = max( trough );
  };

  peak <- double(NROW(trough))
  for (i in 1:NROW(trough)) {
    peak[i] <- max(which(ts.dd[1:trough[i]] == 0));
  };

  recovery.inds = which( ts.max[-(1:trough) ] == as.numeric( ts[ -(1:trough) ] ) );
  if( is.empty( recovery.inds ) ) {
    recovery = NULL;
    recoveryDate = NULL;
  } else {
    recovery = trough + recovery.inds[1];
    recoveryDate = rownames(ts)[recovery];
  };
 
  startDate = rownames(ts)[peak]; 
  endDate = rownames(ts)[trough];
 
  output = list();
  output$maxdrawdown = max.dd

  output$startDate = startDate;
  output$endDate = endDate;
  output$recoveryDate = recoveryDate;

  output$peak = peak;
  output$trough = trough;
  output$recovery = recovery;

  return( output );
};

###########################################################################################
# Routine: maxdrawup.ts
# 
# This function finds the maximum upswing in the time series.
# This is useful for risk management when shorting various assets.
###########################################################################################

maxdrawup.ts <- function( ts, N = NA ) {

  if( !is.na(N) ) {
    # Use a rolling window if N is provided, but front-fill with the
    #    expanding window for the first N-1 observations
    ts.min.expand = rolling.min( ts );
    ts.min = rolling.min( ts, N );
    ts.min[ is.na(ts.min), ] = ts.min.expand[ is.na(ts.min), ];
  } else {
    ts.min = rolling.min( ts );
  };

  ts.dd = ts / ts.min - 1;
  max.dd = max( ts.dd );

  peak = which( max.dd == ts.dd );
  if( 0 == length(peak) || any( as.numeric(ts) < 0 ) ) {
    output = list();
    output$maxdrawup = NA;
    output$startDate = '';
    output$endDate = '';
    output$recoveryDate = '';
    output$beginning = NA;
    output$peak = NA;
    output$recovery = NA;
    return(output);
  } else {
    peak = max( peak );
  };

  beginning <- double(NROW(peak))
  for (i in 1:NROW(peak)) {
    beginning[i] <- max( which( ts.dd[1:peak[i]] == 0 ) );
  };

  recovery.inds = which( ts.min[-(1:peak) ] == as.numeric( ts[ -(1:peak) ] ) );
  if( is.empty( recovery.inds ) ) {
    recovery = NULL;
    recoveryDate = NULL;
  } else {
    recovery = peak + recovery.inds[1];
    recoveryDate = rownames(ts)[recovery];
  };

  startDate = rownames(ts)[beginning];
  endDate = rownames(ts)[peak];

  output = list();
  output$maxdrawup = max.dd;

  output$startDate = startDate;
  output$endDate = endDate;
  output$recoveryDate = recoveryDate;

  output$beginning = beginning;
  output$peak = peak;
  output$recovery = recovery;

  return( output );
};

###########################################################################################
# Routine: avg.return.ts
#
# Gives the average annual return (arithmetic or geometric) over the lifetime of the time series
###########################################################################################

avg.return.ts <- function( ts, type = 'arithmetic' ) {
  nyears = number.of.years( ts );

  if( type == 'arithmetic' ) {
    rtns = returns( ts, method = 'discrete' );
    avg.annual.rtn = colMeans( rtns ) * nrow( rtns ) / nyears;
  } else if ( type == "geometric" ) {
     total.rtn = tail( ts, 1 ) / as.numeric( head( ts, 1 ) );
     avg.annual.rtn = ( total.rtn )^(1/nyears) - 1;
  } else {
     stop( paste( "Unsupported return type: ", type ) ); 
  };

  return( avg.annual.rtn );
};

###########################################################################################
# Routine: number.of.years
#
# Returns the number of years that a time series spans
###########################################################################################

number.of.years <- function( ts ) {
  if( nrow( ts ) == 0 ) {
    nyears = 0;
  } else {
    nyears <- ( as.numeric( max( as.Date( rownames( ts ) ) ) ) - as.numeric( min( as.Date( rownames( ts ) ) ) ) ) / 365.25;
  };

  return( nyears );
};

###########################################################################################
# Routine: stddev.ts
#
# Standard Devation for time series
###########################################################################################

stddev.ts <- function( ts, type = 'levels' ) {
  if( nrow(ts) < 2 ) {
    return( NA );
  };

  if( type == 'levels' ) {
    rtns = returns( ts, method = 'discrete' );
  } else if( type == 'returns' ) {
    rtns = ts;
  } else {
    stop( paste( 'Argument "type" must be equal to "levels" or "returns", not:', type ) );
  };
  
  period.std = apply( rtns, 2, sd );
  periods.per.year = nrow( rtns ) /  number.of.years( rtns );

  annual.std = period.std * sqrt( periods.per.year );
  return( annual.std );
};

###########################################################################################
# Routine: rolling.monthly.avg
#
# Rolling monthly function
###########################################################################################

rolling.monthly.avg <- function(
  ts,
  na.rm = F
) {
  new.ts = rolling.monthly.fun( mean, ts, na.rm = na.rm );
  return( new.ts );
};

###########################################################################################
# Routine: rolling.monthly.fun
#
# Rolling monthly function
###########################################################################################

rolling.monthly.fun <- function(
  fun, 
  ts,
  na.rm = F
) {
  dates = as.Date( rownames( ts ) );
  month.ends = as.Date( get.month.ends( dates[1], tail( dates,1 ) ) );
  vals = numeric( length( month.ends ) ) * NaN;

  vals[1] = apply( matrix( as.numeric( ts[ dates <= month.ends[1],] ), ncol = ncol( ts ) ), 2, fun, na.rm = na.rm ) 
  for( i in 2:length( vals ) ) {
    vals[i] = apply( matrix( as.numeric( ts[ month.ends[i-1] < dates & dates <= month.ends[i],] ), 
								ncol = ncol( ts ) ), 2, fun, na.rm = na.rm ) 
  };
 
  new.ts = timeSeries( vals, month.ends, colnames(ts) );
  return( new.ts );
};

###########################################################################################
# Routine: rolling.beta
###########################################################################################

rolling.beta <- function( 
  rtns.or.symbol, 
  factor.list,
  start.date = last.biz.date() %m-% years(100),
  end.date = last.biz.date(),
  freq = 'D', 
  N = NA, 
  use.proxy = TRUE, 
  is.excess.rtns = FALSE
) {
  if( is.timeSeries( rtns.or.symbol ) ) {
    asset.rtns = rtns.or.symbol;
  } else {
    asset.rtns = ri( get.time.series( rtns.or.symbol, start.date = start.date, end.date = end.date, use.proxy = TRUE ) );
  };

  if( freq == 'M' ) {
    asset.rtns = daily2monthly.rtns( asset.rtns );
  };
   
  factor.rtns = get.factor.ts( factor.list, start.date = start.date, end.date = end.date, freq = freq );
  risk.free.rate = get.factor.ts( 'RF', start.date = start.date, end.date = end.date, freq = freq );

  # Align time series so they are on the same dates, and calculate excess asset returns
  ts = nanmerge( nanmerge( asset.rtns, risk.free.rate ), factor.rtns );

  # If ts already represents excess returns, don't subtract the risk-free rate
  if( is.excess.rtns ) {
    fun = function( ts, na.rm ) as.numeric( lm( ts[,1] ~ ts[,-(1:2)] )$coefficients )
  } else {
    fun = function( ts, na.rm ) as.numeric( lm( ts[,1] - ts[,2] ~ ts[,-(1:2)] )$coefficients )
  };

  betas = roll.fun( fun = fun, ts, N = N, na.rm = F, apply.to.all = FALSE );
  colnames(betas) = c( 'alpha', sprintf( 'beta_%s', colnames( factor.rtns ) ) );

  return( betas );
};

###########################################################################################
# Routine: rolling.lm
#
# Rolling linear regression
###########################################################################################

rolling.lm <- function(
  Y,
  X,
  N = NA
) {
  # Combine X and Y so that we remove dates that aren't contained in both time series
  ts = nanmerge( Y, X );

  fun = function( ts, na.rm ) as.numeric( lm( ts[,1] ~ ts[,-1] )[[1]] );
  roll.lm = roll.fun( fun = fun, ts, N = N, na.rm = F, apply.to.all = FALSE );
  colnames(roll.lm) = c( 'alpha', sprintf( 'beta_%s', colnames( X ) ) );

  return( roll.lm );
};


###########################################################################################
# Routine: rolling.SR
#
# Rolling Sharpe ratio over window size N.  If N = NA, then calculates Sharpe in expanding window
###########################################################################################

rolling.SR <- function(
  ts,
  N = NA, 
  na.rm = F
) {
  rtns = ri( ts, rm.na = F );
  roll.SR = rolling.SR.rtns( rtns, N = N, na.rm = na.rm );
  return( roll.SR );
};

###########################################################################################
# Routine: rolling.SR.rtns
#
# Rolling Sharpe ratio.
###########################################################################################

rolling.SR.rtns <- function(
  rtns,
  N = NA, 
  na.rm = F
) {
  fun = function(x, na.rm ) mean( x, na.rm = na.rm ) / sd( x, na.rm = na.rm );
  roll.SR = roll.fun( fun = fun, rtns, N = N, na.rm = na.rm );
  freq.adj = get.freq.adj( rtns );
  roll.SR = roll.SR * sqrt( freq.adj );
  return( roll.SR );
};

###########################################################################################
# Routine: rolling.avg
###########################################################################################

rolling.avg <- function(
  ts, 
  N = NA
) {
  fun = function(x, na.rm ) mean( x, na.rm = na.rm );
  roll.avg = roll.fun( fun = fun, ts, N = N, na.rm = T );
  return( roll.avg );
};

###########################################################################################
# Routine: get.freq.adj
#
# Get return adjusment based on frequency
###########################################################################################

get.freq.adj <- function( arg ) {
  if( class(arg) == 'character' ) {
    if( arg == 'Y' ) {
      return(1);
    } else if( arg == 'M' ) {
      return( 12 );
    } else if( arg == 'W' ) {
      return( 52 );
    } else if( arg == 'D' ) {
      return( 252 );
    } else {
      stop( paste( 'Unsupported frequency: ', arg ) );
    };
  } else if( class(arg) == 'timeSeries' ) {
    if( frequency(ts) == 1 ) {
      return( 252 );
    } else { 
      return( 12 );
    };
  } else {
    stop( paste( 'Unsupported class in get.freq.adj:', class(arg) ) );
  };
};

###########################################################################################
# Routine: rolling.sd
#
# Rolling standard deviation
###########################################################################################

rolling.sd <- function( 
  ts, 
  N = NA, 
  na.rm = F
) {
  rtns = returns( ts, method = 'discrete', rm.na = F ); 
  roll.std = rolling.sd.rtns( rtns, N = N, na.rm = na.rm );
  return( roll.std );
};

###########################################################################################
# Routine: rolling.vol
#
# Rolling standard deviation (using log returns)
###########################################################################################

rolling.vol <- function(
  ts,
  N = NA,
  na.rm = F
) {
  rtns = returns( ts, method = 'continuous', rm.na = F );
  roll.std = rolling.sd.rtns( rtns, N = N, na.rm = na.rm );
  return( roll.std );
};



###########################################################################################
# Routine: rolling.mom
#
# Rolling M-th Moment of a time series.  For example, M = 1 gives the mean, M = 2, Std.Dev.
###########################################################################################

rolling.mom <- function(
  ts,
  M,
  N = NA,
  na.rm = F
) {
  rtns = ri( ts, rm.na = F );
  roll.mom = rolling.mom.rtns( rtns, M, N = N, na.rm = na.rm );
  return( roll.mom );
};

###########################################################################################
# Routine: rolling.sd.rtns
#
# Rolling standard deviation, with fixed window size N.  
# If N = NA, then the std.dev is calculated with an expanding window.
###########################################################################################

rolling.sd.rtns <- function( 
  rtns, 
  N = NA, 
  na.rm = F 
) {
  roll.std = roll.fun( sd, rtns, N = N, na.rm = na.rm );
  freq.adj = sqrt( get.freq.adj( rtns ) );
  roll.std = roll.std * freq.adj;
  return( roll.std );
};

###########################################################################################
# Routine: rolling.max
#
# Rolling Max
###########################################################################################

rolling.max <- function(
  ts,
  N = NA
) {
  roll.ts = ts;

  for( i in 1:ncol(ts) ) {
    if( !is.na( N ) ) {
      roll.ts[,i] = runMax( ts[,i], n = N );
    } else {
      roll.ts[,i] = runMax( ts[,i], n = 1, cumulative = TRUE );
      roll.ts[1,i] = ts[1,i];
    };
  };

  return( roll.ts );
};

###########################################################################################
# Routine: rolling.min
#
# Rolling Min
###########################################################################################

rolling.min <- function(
  ts,
  N = NA
) {
  roll.ts = -rolling.max( -ts, N = N );
  return( roll.ts );
};


###########################################################################################
# Routine: rolling.sum
#
# Rolling Sum
###########################################################################################

rolling.sum <- function(
  ts,
  N = NA
) {
  roll.ts = ts;

  for( i in 1:ncol(ts) ) {
    if( !is.na( N ) ) {
      roll.ts[,i] = runSum( ts[,i], n = N );
    } else {
      roll.ts[,i] = runSum( ts[,i], n = 1, cumulative = TRUE );
      roll.ts[1,i] = ts[1,i];
    };
  };

  return( roll.ts );
};

###########################################################################################
# Routine: rolling.mom.rtns
#
# Rolling M-th moment of time series, with fixed window size N.  
# If N = NA, then it is calculated with an expanding window.
# For example, when M = 1, this calculates rolling Mean.  M=2 calculates rolling Std.Dev.
###########################################################################################

rolling.mom.rtns <- function(
  rtns,
  M,
  N = NA,
  na.rm = F
) {
  moment.fun = function(x, na.rm ) moment( x, order = M, central = TRUE, na.rm = na.rm ) / sd( x )^M;
  roll.mom = roll.fun( moment.fun, rtns, N = N, na.rm = na.rm );
  return( roll.mom );
};

###########################################################################################
# Routine: rolling.cor.rtns
#
# Rolling correlation with fixed window size N.
# If N = NA, then the corr is calculated with an expanding window.
###########################################################################################

rolling.cor.rtns <- function(
  rtns1,
  rtns2,
  N = NA,
  na.rm = F
) {
  rtns = nanmerge( rtns1, rtns2 );
  fun = function( x, na.rm ) cor(x[,1], x[,2] );
  roll.cor = roll.fun( fun, rtns, N = N, apply.to.all = FALSE )[,1];
  return( roll.cor );
};


###########################################################################################
# Routine: roll.fun
#
# Apply a function to a time series in a rolling window
###########################################################################################

roll.fun <- function( 
  fun, 
  ts, 
  N = NA,
  na.rm = F, 
  apply.to.all = T
) {
  if( !is.na( N ) ) {  
    for( i in N:nrow( ts ) ) {
      if( apply.to.all ) {
        new.row = apply( matrix( as.numeric( ts[ (i-N+1):i,] ), ncol = ncol( ts ) ), 2, fun, na.rm = na.rm )
      } else {
        new.row = fun( matrix( as.numeric( ts[ (i-N+1):i,] ), ncol = ncol( ts ) ), na.rm = na.rm );
      };

      if( i == N ) {
        new.ts = timeSeries( matrix( NaN, nrow = nrow(ts)-N+1, ncol = length( new.row ) ), rownames( ts )[N:nrow(ts)] );
      };

      new.ts[i-N+1,] = new.row;
    }
  } else {
    for( i in 1:nrow( ts ) ) {
      if( apply.to.all ) {
        new.row = apply( matrix( as.numeric( ts[ 1:i,] ), ncol = ncol( ts ) ), 2, fun, na.rm = na.rm )
      } else {
        new.row = fun( matrix( as.numeric( ts[ 1:i,] ), ncol = ncol( ts ) ), na.rm = na.rm );
      }

      if( i == 1 ) {
        new.ts = timeSeries( matrix( NaN, nrow = nrow(ts), ncol = length( new.row ) ), rownames( ts ) );
      };

      new.ts[i,] = new.row;
    }
  };

  colnames( new.ts ) = colnames(ts);
  return( new.ts );
};



###########################################################################################
# Routine: SharpeRatio.ts
#
# Sharpe Ratio for levels (annualized)
###########################################################################################

SharpeRatio.ts <- function( ts ) {
  if( nrow( ts ) < 2 ) {
    return( rep( NA, ncol( ts ) ) );
  } else {
    rtns = ri( ts );
    SR = SharpeRatio.rtn( rtns );
    return( SR );
  };
};

###########################################################################################
# Routine: SharpeRatio.rtn
#
# Sharpe Ratio for returns (annualized)
###########################################################################################

SharpeRatio.rtn <- function( rtns ) {
  if( nrow( rtns ) < 2 ) {
    return( rep( NA, ncol( rtns ) ) );
  } else {
    freq.adj = sqrt( get.freq.adj( rtns ) );
    SR = apply( rtns, 2, mean, na.rm = T ) / apply( rtns, 2, sd, na.rm = T ) * freq.adj;
    return( SR );
  };
};

###########################################################################################
# Routine: fill.missing
#
# Add all missing dates to a time series, and then replace NA's with the last available value
###########################################################################################

fill.missing <- function(
  ts, 
  start.date = first.date( ts ), 
  end.date   = last.date( ts ), 
  fill.qty   = '', 
  only.weekdays = FALSE
) {
  missing.dates <- as.Date( setdiff( as.Date( start.date ):as.Date( end.date ), as.Date( rownames( ts ) ) ) );
  colnames(ts) = gsub( '[][]', '', colnames( ts ) );

  if( fill.qty == '' ) {
    if( length( missing.dates ) > 0 ) { 
      data      <- matrix( NA, nrow = length( missing.dates ), ncol = ncol( ts ) );
      filled.ts <- merge( ts, timeSeries( data, missing.dates, units = colnames( ts ) ) );
      new.ts    <- na.locf( filled.ts );
      colnames( new.ts ) = colnames(ts);
    } else {
      new.ts  <- na.locf( ts );
    };
  } else {
    data   <- matrix( fill.qty, nrow = length( missing.dates ), ncol = ncol( ts ) );
    new.ts <- merge( ts, timeSeries( data, missing.dates, units = colnames( ts ) ) );
    colnames( new.ts ) = colnames(ts);
  };

  if( only.weekdays ) {
    new.ts = new.ts[ wday( rownames( new.ts ) ) %in% 2:6, ];
  };
   
  return( new.ts );
};

###########################################################################################
# Routine: merge.ts 
#
# Merge a list of time series, and optionally remove NA's
###########################################################################################

merge.ts <- function( inputs, input.ts1 = NULL, rm.na = FALSE ) { 
   if( !is.null( input.ts1 ) ) {
     input.ts = list( inputs, input.ts1 );
   } else {
     input.ts = inputs;
   };

   empty.inds = unlist( lapply( input.ts, function(x) is.empty(x) ) );
   all.ts = input.ts[ !empty.inds ];

   if( is.empty( all.ts ) ) {
     return( NULL );
   } else if( length( all.ts ) == 1 )  {
     return( all.ts[[1]] );
   } else { 
     ts = all.ts[[1]];
     for( i in 2:length( all.ts ) ) {
       ts = cbind( ts, all.ts[[i]] );
     };

     if( rm.na == TRUE ) {
       ts = ts[ !is.na( rowSums( ts ) ), ];
     };
   };
 
   return( ts );
};

###########################################################################################
# Routine: beta.from.levels
#
# Given an asset's levels and the market's levels, calculate the asset's beta to the market
###########################################################################################

beta.from.levels <- function( 
  asset.levels, 
  market.levels, 
  start.date = first.date( asset.levels ),
  end.date   = last.date( asset.levels ),
  rtn.method = "discrete"
) {
  rtns = ri( merge.ts( list( asset.levels, market.levels ), rm.na = TRUE ), rm.na = TRUE );

  reg = lm( rtns[, 1 ] ~ rtns[, 2 ] );
  return( as.numeric( reg$coefficient[2] ) ); 
};

###########################################################################################
# Routine: ind.ts
#
# Normalize time series to start at one. 
###########################################################################################

ind.ts <- function( ts ) {
  new.ts <- ts * NaN;
  for( i in 1:ncol( ts ) ) {
    first.val.loc = min( which( is.na( ts[, i ] ) == FALSE ) );
    new.ts[, i ] = ts[, i ] / as.numeric( ts[ first.val.loc, i ] );
  };

  return( new.ts );
};

###########################################################################################
# Routine: calc.returns 
#
# Given a time series object, calculate the returns
###########################################################################################

calc.returns <- function( ts, nday.rtn = 1, rm.na = TRUE ) 
{
  if( is.null( nrow( ts ) ) ) {
    rtns = -1 + ts[ -(1:nday.rtn) ] / ts[ 1:(length( ts ) - nday.rtn ) ];
  
    if( rm.na == FALSE ) {
      rtns = c( rep( NA, nday.rtn ), rtns ); 
    };
      
  } else {
    rtns = -1 + ts[ -(1:nday.rtn), ] / matrix( as.numeric( ts[ 1:(nrow(ts) - nday.rtn ), ] ), ncol = ncol( ts ) );
  
    if( rm.na == FALSE ) {
       rtns = rbind( matrix( NA, ncol = ncol( rtns ), nrow = nday.rtn ), rtns );
       rownames( rtns ) = rownames( ts );
       colnames( rtns ) = colnames( ts );
    };
  };

  return( rtns );
};

###########################################################################################
# Routine: di
#
# Given a time series object, calculate the differences
###########################################################################################

di <- function( ts, n = 1 ) {
  dts = ts[ -(1:n),] - as.numeric( ts[ 1:(nrow(ts)-n),] );
  return( dts );
};


###########################################################################################
# Routine: ri
#
# Given a time series object, calculate the returns
###########################################################################################

ri <- function( ts, n = 1, rm.na = TRUE ) {
  rtns = calc.returns( ts, nday.rtn = n, rm.na = rm.na );
  return( rtns );
};

###########################################################################################
# Routine: prod.ts
#
# Given a time series object, of returns, calculate the product
###########################################################################################

prod.ts <- function( rtns, fill.missing = TRUE, method = 'discrete' ) {
 
  if( fill.missing == TRUE ) {   
    r = rtns;
    r[ is.na( r ) ] = 0;

    levels = cumulated( r, method = method );
    ncols = ifelse( is.null( ncol( rtns ) ), 1, ncol( rtns ) );

    for( i in 1:ncols ) {

      first.entry = min( which( !is.na( rtns[, i ] ) ) );

      if( first.entry > 1 )
        levels[ 1:first.entry, i ] = rep( NA, 1, first.entry );
    };     
  } else {
    levels = cumulated( rtns, method = method );
  };
  
  return( levels );
};

###########################################################################################
# Routine: create.ts.row
#
# From a vector of values and names, create a timeSeries with multiple columns and one date
###########################################################################################

create.ts.row <- function( 
  vec, 
  date, 
  names
)
{
  if( length( vec ) != length( names ) ) { 
    stop( "Length of 'vec' must be the same as length of 'names'" );
  };

  return( timeSeries( matrix( vec, ncol = length( names ), nrow = 1  ), date, units = names ) );
};

###########################################################################################
# Routine: ts.sum
###########################################################################################

ts.sum <- function( ts1, ts2 ) {

  if( is.null( rownames( ts1 ) ) ) {
    return( ts2 )
  } else if( is.null( rownames( ts2 ) ) ) {
    return( ts1 ) 
  };

  dates1 <- rownames( ts1 );
  dates2 <- rownames( ts2 );

  missing.dates1 <- setdiff( dates2, dates1 )
  missing.dates2 <- setdiff( dates1, dates2 )

  cols   <- sort( unique( c( colnames( ts1 ), colnames( ts2 ) ) ) );

  if( length( missing.dates1 ) > 0 ) {
    zeros1 <- timeSeries( matrix( 0, nrow = length( missing.dates1 ), ncol = length( cols ) ), missing.dates1, units = cols );
    ts1 <- merge( ts1, zeros1 ); 
    ts1[ is.na( ts1 ) ] = 0
  };

  if( length( missing.dates2 ) > 0 ) {
    zeros2 <- timeSeries( matrix( 0, nrow = length( missing.dates2 ), ncol = length( cols ) ), missing.dates2, units = cols );
    ts2 <- merge( ts2, zeros2 ); 
    ts2[ is.na( ts2 ) ] = 0
  };

  total <- ts1[, cols ] + ts2[, cols ];
  return( total );
};

##########################################################################################################
# Routine: ts.range
#
# Restrict a time series to lie between the start and end dates
##########################################################################################################

ts.range <- function( 
  ts, 
  start.date = first.date( ts ), 
  end.date = last.date( ts )
) {
  new.ts = ts[ start.date <= as.Date( rownames( ts ) ) & as.Date( rownames( ts ) ) <= end.date, ];
  return( new.ts );
};

##########################################################################################################
# Routine: name2label
#
# Remove special characters from names to give proper time series labels
##########################################################################################################

name2label <- function( symbols ) {
  labels = gsub( '\\^', '', symbols );
  return( labels );
};

##########################################################################################################
# Routine: label2name
#
# Given time series labels and the set of original symbol names, map the labels to symbols
##########################################################################################################

label2name <- function( labels, symbols ) {
  matched.symbols = symbols[ match( name2label( symbols ), labels ) ];
  return( matched.symbols );
};


##########################################################################################################
# Routine: get.historical.drawdowns
#
# Given a time series, find the drawdowns and Start, End, and Trough dates.
##########################################################################################################

get.historical.drawdowns <- function( ts ) {
  dates = as.Date( rownames( ts ) );

  mdd = mdd.ts( ts );

  start.date = end.date = trough.date = c();
  drawdowns = data.frame();

  nDrawdowns = 0;
  is.downdraft = FALSE;
  for( i in 1:nrow( mdd ) ) {
    if( !is.downdraft && mdd[i] > 0 ) {
      is.downdraft = TRUE;
      nDrawdowns = nDrawdowns + 1;
      if( nDrawdowns == 1 ) {
        drawdowns = data.frame( StartDate = dates[i-1], TroughDate = dates[i], EndDate = NA, Drawdown = mdd[i] );
      } else {
        drawdowns[ nDrawdowns, ] = data.frame( StartDate = dates[i-1], TroughDate = dates[i], EndDate = NA, Drawdown = mdd[i] );
      };
    } else if( is.downdraft ) {
      if( mdd[i] == 0 ) {
        is.downdraft = FALSE;
        drawdowns[ nDrawdowns, 'EndDate' ] = as.Date( dates[i] );
      } else if( drawdowns[ nDrawdowns, 'Drawdown' ] < mdd[i] ) {
        drawdowns[ nDrawdowns, 'Drawdown' ] = mdd[i];
        drawdowns[ nDrawdowns, 'TroughDate' ] = dates[i];
      };
    };
  };

  drawdowns$EndDate = as.Date( drawdowns$EndDate )
  drawdowns = drawdowns[ order( drawdowns$Drawdown, decreasing = TRUE ), ];
  rownames( drawdowns ) = NULL;
  return( drawdowns );
};

##########################################################################################################
# Routine: insert.dates.ts
#
# Given a time series, insert empty rows
##########################################################################################################

insert.dates.ts <- function(
  ts, 
  insert.dates, 
  fill.value = NA
) {
  new.dates = as.Date( setdiff( as.Date( insert.dates ), as.Date( rownames( ts ) ) ) );

  if( length( new.dates ) > 0 ) {
    new.data = matrix( fill.value, ncol = ncol(ts), nrow = length(new.dates) );
    new.ts = timeSeries( new.data, new.dates, units = colnames(ts) );
    output.ts = merge( ts, new.ts );
  } else {
    output.ts = ts;
  };

  return( output.ts );
};


