
setClass(  
   "Simulation", 
  representation( 
      Symbols = "character", 
      FactorList = 'character', 
      NumberOfSimulations = 'numeric',
      NumberOfPeriods = 'numeric', 
      BlockSize = 'numeric', 
      NoiseType = 'character', 
      Frequency = 'character', 
      StartDate = 'Date', 
      EndDate   = 'Date', 
      Method = 'character',

      Returns = 'matrix'
   ), 
);

setMethod( "initialize", "Simulation",
   function( .Object, symbols = c(), factor.list = 'MKT', num.sims = 1e4, nperiods = 21, block.size = 5, noise.type = 'normal', 
						freq = 'D', method = 'Block',
					start.date = today() %m-% years(30), end.date = last.biz.date() ) {
     .Object@Symbols <- symbols;
     .Object@FactorList = factor.list;
     .Object@StartDate = start.date;
     .Object@EndDate = end.date;
     .Object@NumberOfSimulations = num.sims;
     .Object@NumberOfPeriods = nperiods;
     .Object@BlockSize = block.size;
     .Object@NoiseType = noise.type;
     .Object@Frequency = freq;
       
      if( method != 'Block' ) {
        stop( sprintf( 'Unsupported Method %s: only Block bootstrap simulations are currently supported', method ) );      
      } else {
        .Object@Method = method;
      };

      return( .Object );
    }
);

setGeneric( "setReturns", def=function( .Object, recalculate = FALSE ) standardGeneric( "setReturns" ) );
setMethod(  "setReturns", "Simulation", 
  function( .Object, recalculate = FALSE ) {
    symbols = .Object@Symbols;
    factor.list = .Object@FactorList;
    start.date = .Object@StartDate;
    end.date = .Object@EndDate;
    freq = .Object@Frequency;
    nperiods = .Object@NumberOfPeriods;
    block.size = .Object@BlockSize;
    num.sims = .Object@NumberOfSimulations;
    noise.type = .Object@NoiseType;
    method = .Object@Method;

    if( is.empty( Returns( .Object ) ) || recalculate == TRUE ) {
      if( method != 'Block' ) {
        stop( sprintf( 'Unsupported method %s: only block bootstrap is currently supported', method ) );
      } else {
         .Object@Returns = simulate.from.factors( symbols, factor.list = factor.list, start.date = start.date, end.date = end.date, 
	 	freq = freq, nperiods = nperiods, block.size = block.size, num.sims = num.sims, noise.type = noise.type );
      };
    };

    return( .Object );
  }
);

setGeneric( 'Returns', def=function( .Object ) standardGeneric( 'Returns' ) );
setMethod( 'Returns', 'Simulation', function( .Object ) .Object@Returns );


