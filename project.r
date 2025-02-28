library(quantmod)
library(tidyverse)
library(dplyr)
library(zoo)

# food companies

nestle <- getSymbols("NESN.SW", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
nestle <- nestle$NESN.SW.Close
head(nestle)

unilever <- getSymbols("UL", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
unilever <- unilever$UL.Close
head(unilever)

danone <- getSymbols("BN.PA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
danone <- danone$BN.PA.Close
head(danone)

bonduelle <- getSymbols("BON.PA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
bonduelle <- bonduelle$BON.PA.Close
head(bonduelle)


pepsi <- getSymbols("PEP", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
pepsi <- pepsi$PEP.Close
head(pepsi)

mcdonalds <- getSymbols("MCD", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
mcdonalds <- mcdonalds$MCD.Close
head(mcdonalds)

kelloggs <- getSymbols("K", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
kelloggs <- kelloggs$K.Close
head(kelloggs)


kraft_heinz <- getSymbols("KHC", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
kraft_heinz <- kraft_heinz$KHC.Close
head(kraft_heinz)



# travel 

trivago <- getSymbols("TRVG", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
trivago <- trivago$TRVG.Close
head(trivago)

booking <- getSymbols("BKNG", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
booking <- booking$BKNG.Close
head(booking)

ryanair <- getSymbols("RYA.IR", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
ryanair <- ryanair$RYA.IR.Close
head(ryanair)

ryanair <- getSymbols("RYA.IR", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
ryanair <- ryanair$RYA.IR.Close
head(ryanair)


airbnb <- getSymbols("ABNB", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
airbnb <- airbnb$ABNB.Close
head(airbnb)

trip.com <- getSymbols("TCOM", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
trip.com <- trip.com$TCOM.Close
head(trip.com)

# oil and gas

shell <- getSymbols("SHEL", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
shell <- shell$SHEL.Close
head(shell)


eni <- getSymbols("ENI.MI", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
eni <- eni$ENI.MI.Close
head(eni)

enel <- getSymbols("ENEL.MI", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
enel <- enel$ENEL.MI.Close
head(enel)


engie <- getSymbols("ENGI.PA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
engie <- engie$ENGI.PA.Close
head(engie)


orsted <- getSymbols("ORSTED.CO", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
orsted <- orsted$ORSTED.CO.Close
head(orsted)


# logistic

zalando <- getSymbols("ZAL.DE", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
zalando <- zalando$ZAL.DE.Close
head(zalando)

ups <- getSymbols("UPS", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
ups <- ups$UPS.Close
head(ups)

amazon <- getSymbols("AMZN", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
amazon <- amazon$AMZN.Close
head(amazon)

dhl <- getSymbols("DHL.DE", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
dhl <- dhl$DHL.DE.Close
sum(is.na(dhl))
dhl[] <- lapply(dhl, function(col) na.fill(col, "extend"))
sum(is.na(dhl))
head(dhl)


fedex <- getSymbols("FDX", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
fedex <- fedex$FDX.Close
head(fedex)
 

maersk <- getSymbols("AMKBY", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
maersk <- maersk$AMKBY.Close
head(maersk)

maersk <- getSymbols("AMKBY", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
maersk <- maersk$AMKBY.Close
head(maersk)


walmart <- getSymbols("WMT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
walmart <- walmart$WMT.Close
head(walmart)

 

# technology

spotify <- getSymbols("SPOT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
spotify <- spotify$SPOT.Close
head(spotify)


netflix <- getSymbols("NFLX", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
netflix <- netflix$NFLX.Close
head(netflix)


nvidia <- getSymbols("NVDA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
nvidia <- nvidia$NVDA.Close
head(nvidia)


meta <- getSymbols("META", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
meta <- meta$META.Close
head(meta)



apple <- getSymbols("AAPL", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
apple <- apple$AAPL.Close
head(apple)


samsung <- getSymbols("005930.KS", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
samsung <- samsung$`005930.KS.Close`
head(samsung)


microsoft <- getSymbols("MSFT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
microsoft <- microsoft$MSFT.Close
head(microsoft)

google <- getSymbols("GOOG", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
google <- google$GOOG.Close
head(google)



# automobile companies

Volkswagen <- getSymbols("VOW3.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Volkswagen <- Volkswagen$VOW3.DE.Close
head(Volkswagen)


Ferrari <- getSymbols("RACE.MI", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Ferrari <- Ferrari$RACE.MI.Close
head(Ferrari)

Stellantis <- getSymbols("STLAM.MI", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Stellantis <- Stellantis$STLAM.MI.Close
head(Stellantis)



Renault <- getSymbols("RNO.PA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Renault <- Renault$RNO.PA.Close
head(Renault)


Mercedes_Benz_Group <- getSymbols("MBG.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Mercedes_Benz_Group <- Mercedes_Benz_Group$MBG.DE.Close
head(Mercedes_Benz_Group)

BMW <- getSymbols("BMW.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
BMW <- BMW$BMW.DE.Close
head(BMW)


tesla <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
tesla <- tesla$TSLA.Close
head(tesla)


toyota <- getSymbols("TM", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
toyota <- toyota$TM.Close
head(toyota)


