
###########################################################################################
# Routine: estimate.dividend.yield
#
# Estimate dividend yield, assuming that the dividend is paid either quarterly, 
# semi-annually, or annually. If the frequency of payment appears to change, then we just
# use the sum of the past year's dividend to determine the yield
###########################################################################################

estimate.dividend.yield <- function(
  symbol,
  asOfDate = last.biz.date( today() ),
  levels   = c()
) {
  divs = get.dividends( symbol, start.date = today() %m-% years(5) );
  divs = divs[ order( as.Date( divs$Date ) ), ];

  if( identical( levels, c() ) )
    levels = get.time.series( symbol, data.type = 'Close', start.date = asOfDate %m-% years(5) );

  # Assume dividends occur with frequencies of 3, 6, or 12 months
  freqs = round(  abs( 365 / diff( as.numeric( as.Date( divs$Date ) ) ) ) );

  if( length( freqs ) == 0 )
    return(0);

  if( identical( median( freqs ), unique( tail( freqs, median( freqs ) ) ) ) ) {
    annual.dividend = tail( divs$Dividends, 1 ) * median( freqs );
  } else {
    annual.dividend = sum( divs[  asOfDate %m-% years(1) <= as.Date( divs$Date ) & as.Date( divs$Date ) <= asOfDate, 'Dividends' ] );
  };

  dividendYield = annual.dividend / as.numeric( levels[ tail( as.Date( divs$Date ), 1 ) ] );
  payment.dates = last.biz.date( max( as.Date( divs$Date ) ) %m+% months( round(12/median(freqs) ) * (1:( median( freqs ) * 5 ) ) ) );

  div.info = list( dividendYield, payment.dates );
  return( div.info );
};


