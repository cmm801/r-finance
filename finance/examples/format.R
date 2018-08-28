
df = data.frame( Symbol = c( 'ABC', 'DEF', 'HIJ', 'BBB', 'CCC' ), Date = today() + 1:5, 
	SharpeRatio = rnorm(5), Quantity = runif(5) * 100, 
	MarketValue = runif(5) * 1000000, Price = runif(5) * 100  );

FMT = list();
FMT[[2]] = 'MMDDYYYY';
FMT[[3]] = 'SR';
FMT[[4]] = 'Quantity';
FMT[[5]] = 'MarketValue';
FMT[[6]] = get.format( 'Generic', decimals = 2 );

formatted.df = format.df( df, FMT );

fmt.hash = hash();
fmt.hash$Date = 'MMDDYYYY';
fmt.hash$SharpeRatio = 'SR';
fmt.hash$MarketValue = 'MarketValue';
fmt.hash$Price = 'Price';
fmt.hash$Quantity ='Quantity';

hash.df = format.df( df, fmt.hash );

