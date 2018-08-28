
###########################################################################################
# Routine: get.period.ends
#
# Get all dates between start and end date, given a particular frequency
###########################################################################################

get.period.ends <- function( 
  start.date, 
  end.date, 
  freq
) {
  if( freq == 'Y' ) {
    month.ends = get.month.ends( start.date, end.date );
    dates = month.ends[ month( month.ends ) == 12 ];
  } else if( freq == 'M' ) {
    dates = get.month.ends( start.date, end.date );
  } else if( freq == 'W' ) {
    all.dates = as.Date( start.date:end.date );
    dates = all.dates[ wday( all.dates ) == 6 ];
  } else if( freq == 'D' ) {
    dates = as.Date( start.date:end.date );
  } else {
    stop( paste( 'Unsupported frequency: ', freq ) )
  };

  return( as.Date( dates ) );
};

###########################################################################################
# Routine: get.month.ends
###########################################################################################

get.month.ends <- function( start.date, end.date ) {
  dates = as.Date( as.Date( start.date ):as.Date( end.date ) );
  month.ends = yearMonth2date( unique( date2yearMonth( dates ) ) )
  return( month.ends );
};

###########################################################################################
# Routine: get.year.ends
###########################################################################################

get.year.ends <- function( start.date, end.date ) {
  month.ends = get.month.ends( start.date, end.date );
  eoy.dates = month.ends[ month( month.ends ) == 12 ];
  return( eoy.dates );
};

###########################################################################################
# Routine: date2yearMonth
#
# Convert Date to yearMonth
###########################################################################################

date2yearMonth <- function( date ) {
  yearMonth = 100 * year( date ) + month( date );
  return( yearMonth );
};

###########################################################################################
# Routine: yearMonth2date
#
# Convert yearMonth to Date
###########################################################################################

yearMonth2date <- function( yearMonth, method = 'last' ) {
  if( method == 'first' ) {
    date = as.Date( sprintf( '%d-%02d-01', round( yearMonth / 100 ), yearMonth %% 100 ) );
  } else if( method == 'last' ) {
    date = as.Date( sprintf( '%d-%02d-01', round( yearMonth / 100 ), yearMonth %% 100 ) );
    date = date %m+% months(1) - 1;
  } else {
    stop( paste( 'Unsupported method: ', method ) );
  };

  return( date );
};

###########################################################################################
# Routine: find.all.period.ends
#
# Finds year, month, or week ends falling between two dates.  Assumes that weeks end on Friday
###########################################################################################

find.all.period.ends <- function( start.date, end.date, period = 'month' ) {
  dates = as.Date( start.date:(end.date+1) );

  if( period == 'month' ) {
    end.locs = which( diff( month( dates ) ) != 0 );
  } else if( period == 'year' ) {
    end.locs = which( diff( year( dates ) ) != 0 ); 
  } else if( period == 'week' ) {
    end.locs = which( diff( as.numeric( wday( dates ) == 6 ) ) == -1 );
  };

  end.locs  = end.locs[ end.locs <= length( start.date:end.date ) ];
  end.dates = dates[ end.locs ];
  return( end.dates );
};

###########################################################################################
# Routine: get.holidayNYSE
#
# Use the code from the timeDate package for holidayNYSE.  This fixes the bugs that occur
# from converting the timeDates to the Date class. 
###########################################################################################

get.holidayNYSE <- function( start.year, end.year = year( today() ) ) {
  holidays <- NULL
  for (y in start.year:end.year) {
       if (y >= 1885) 
            holidays <- c(holidays, as.character(USNewYearsDay(y)))
       if (y >= 1885) 
            holidays <- c(holidays, as.character(USIndependenceDay(y)))
       if (y >= 1885) 
            holidays <- c(holidays, as.character(USThanksgivingDay(y)))
       if (y >= 1885) 
            holidays <- c(holidays, as.character(USChristmasDay(y)))
       if (y >= 1887) 
            holidays <- c(holidays, as.character(USLaborDay(y)))
       if (y != 1898 & y != 1906 & y != 1907) 
            holidays <- c(holidays, as.character(USGoodFriday(y)))
       if (y >= 1909 & y <= 1953) 
            holidays <- c(holidays, as.character(USColumbusDay(y)))
       if (y >= 1998) 
            holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
       if (y >= 1896 & y <= 1953) 
            holidays <- c(holidays, as.character(USLincolnsBirthday(y)))
       if (y <= 1970) 
            holidays <- c(holidays, as.character(USWashingtonsBirthday(y)))
       if (y > 1970) 
            holidays <- c(holidays, as.character(USPresidentsDay(y)))
       if (y == 1918 | y == 1921 | (y >= 1934 & y <= 1953)) 
            holidays <- c(holidays, as.character(USVeteransDay(y)))
       if (y <= 1968 | y == 1972 | y == 1976 | y == 1980) 
            holidays <- c(holidays, as.character(USElectionDay(y)))
       if (y <= 1970) 
            holidays <- c(holidays, as.character(USDecorationMemorialDay(y)))
       if (y >= 1971) 
            holidays <- c(holidays, as.character(USMemorialDay(y)))
  }

  holidays <- as.Date( sort(holidays) );

  return( holidays );
};

###########################################################################################
# Routine: as.Date.yyyymmdd
#
# Convert a string with format yyyymmdd format
###########################################################################################

as.Date.yyyymmdd <- function( dt ) {
  # Remove any separators between the days, months, years
  new.dt = gsub( '[-/.]', '', dt );

  chars  = strsplit( new.dt, '' )[[1]];

  if( length(chars) != 8 ) {
    return( NA );
  };

  Year  = as.numeric( paste( chars[1:4], collapse = '' ) );
  Month = as.numeric( paste( chars[5:6], collapse = '' ) );
  Day   = as.numeric( paste( chars[7:8], collapse = '' ) );

  date = as.Date( sprintf( '%d-%d-%d', Year, Month, Day ) );
  return( date );
};

###########################################################################################
# Routine: DateRuleApply 
#
# Provides the means to add days, weeks, months, years to a date.  
###########################################################################################

DateRuleApply <- function( date, Rdate ) {
  if( Rdate == '' ) {
     return( date )
  } else if( sum( grep( 'd', Rdate ) ) ) {
    by = sprintf( '%d days', as.numeric( gsub( 'd', '', Rdate ) ) );
  } else if( sum( grep( 'b', Rdate ) ) ) {
    target = as.numeric( gsub( 'b', '', Rdate ) );
    b.days = 0;
    all.days = 0;
    while( b.days != target & abs(b.days) < 1e4 ) {
      if( isBizday( date2timeDate( date + sign(target) * days(all.days) ) ) ) {
        b.days = b.days + sign(target) * 1;
      };

      all.days = all.days + sign(target);
    };
    by = sprintf( '%d days', all.days );
  } else if( sum( grep( 'w', Rdate ) ) ) {
    by = sprintf( '%d weeks', as.numeric( gsub( 'w', '', Rdate ) ) );
  } else if( sum(grep( 'm', Rdate ) ) ) {
    by = sprintf( '%d months', as.numeric( gsub( 'm', '', Rdate ) ) );
  } else if( sum(grep( 'y', Rdate ) ) ) {
    by = sprintf( '%d years', as.numeric( gsub( 'y', '', Rdate ) ) );
  } else if( sum(grep( 'q', Rdate ) ) ) {
    by = sprintf( '%d months', 3*as.numeric( gsub( 'q', '', Rdate ) ) );
  } else {
    print( sprintf( 'The Rdate %s is not supported', Rdate ) );
  };

  new.date = as.Date( seq( from = as.Date( date ), by = by, length = 2 ) )[2];
  return( new.date );
};

###########################################################################################
# Routine: last.biz.date
#
# Given a date, returns the previous business day (or the date, if it is a business day)
###########################################################################################

last.biz.date <- function( date = today() ) {
  holidays = get.holidayNYSE( min( year( date ) ) - 1, max( year( date ) ) );

  asOfDate = date;
  for( i in 1:length( date ) ) {
    tmp = date[i];
    while( !isBizday( timeDate( tmp + hours(23) ), holidays = holidays )[[1]] )
      tmp = tmp - days(1);
    
    asOfDate[i] = tmp;
  };

  return( asOfDate );
};

###########################################################################################
# Routine: date2timeDate
#
# Convert Date to timeDate
###########################################################################################

date2timeDate <- function( date ) {
  return( timeDate( date + hours( 23 ) ) );
};

###########################################################################################
# Routine: timeDate2Date
#
# Convert timeDate to Date
###########################################################################################

timeDate2Date <- function( time.date ) {
  return( as.Date( format( time.date, '%Y-%m-%d' ) ) );
};

###########################################################################################
# Routine: get.all.cal.days
#
# Get all calendar days between start and end dates
###########################################################################################

get.all.cal.days <- function( start.date, end.date ) {
  if( start.date > end.date ) {
    return( c() )
  } else {
    return( as.Date( start.date ):as.Date( end.date ) );
  };
};

###########################################################################################
# Routine: count.cal.days
###########################################################################################

count.cal.days <- function( start.date, end.date ) {
  days = get.all.cal.days( start.date = start.date, end.date = end.date );
  return( length( days ) );
};

###########################################################################################
# Routine: get.all.biz.days
#
# Get all business dates between start and end dates
###########################################################################################

get.all.biz.days <- function( start.date, end.date, calendar = 'NYSE' ) {
  dates = get.all.cal.days( start.date, end.date );

  if( length( dates ) == 0 ) 
    return( c() );
  
  if( calendar == 'NYSE' ) {
    is.holiday = !is.na( match( dates, get.holidayNYSE( year( start.date ), year( end.date ) ) ) );
    is.weekday = wday( dates ) != 7 & wday( dates ) != 1;
    biz.dates = dates[ is.weekday & !is.holiday ];
  } else {
    stop( sprintf( 'Calendar %s is not supported', calendar ) );
  };

  return( biz.dates );
};

###########################################################################################
# Routine: count.biz.days
###########################################################################################

count.biz.days <- function( start.date, end.date ) {
  days = get.all.biz.days( start.date = start.date, end.date = end.date );
  return( length( days ) );
};


###########################################################################################
# Routine: str2date
###########################################################################################

str2date <- function( dateStrings ) {
  dates = character( length( dateStrings ) );
  for( i in 1:length( dateStrings ) ) {
    dateString = dateStrings[i];
    L = nchar( dateString );
    year = substr( dateString, L-3, L );
    month = which( month.abb == substr( dateString, 1, 3 ) );
    day = substr( dateString, 5, L-6 );
    dates[i] = sprintf( '%s-%s-%s', year, month, day  );
  };

  dates = as.Date( dates );
  return( dates );
};


