## Import danych z funduszy inwestycyjnych

## W katalogu ../fundusze/mstfun pliki z notowaniami funduszy z
## http://bossa.pl/pub/fundinwest/mstock/mstfun.zip

## Lista funduszy (plik tekstowy)
## http://bossa.pl/pub/fundinwest/mstock/mstfun.lst
require(zoo)

fund     <- "DWS002.mst" # wpisz nazwe funduszu
raw.data <- read.csv2(file = fund, head=TRUE, sep=",", dec=".")
head(raw.data)
dateForm <- "%Y%m%d"

## konwersja do zoo
prices <- raw.data$"X.OPEN."
dates  <- as.Date(as.character(raw.data$"X.DTYYYYMMDD."), format=dateForm)

P      <- zoo(prices , order.by = dates)
r      <- diff(log(P))

plot(P)
