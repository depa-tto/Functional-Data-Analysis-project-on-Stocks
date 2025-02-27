library(quantmod)
library(tidyverse)
library(dplyr)

# food companies

nestle <- getSymbols("NESN.SW", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
nestle <- nestle$NESN.SW.Close
head(nestle)

unilever <- getSymbols("HINDUNILVR.NS", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
unilever <- unilever$HINDUNILVR.NS.Close
head(unilever)

danone <- getSymbols("BN.PA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
danone <- danone$BN.PA.Close
head(danone)

bonduelle <- getSymbols("BON.PA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
bonduelle <- bonduelle$BON.PA.Close
head(bonduelle)


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


# technology

spotify <- getSymbols("SPOT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
spotify <- spotify$SPOT.Close
head(spotify)


siemens <- getSymbols("SIE.DE", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
siemens <- siemens$SIE.DE.Close
head(siemens)


airbus <- getSymbols("AIR.PA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
airbus <- airbus$AIR.PA.Close
head(airbus)


leonardo <- getSymbols("LDO.MI", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
leonardo <- leonardo$LDO.MI.Close
head(leonardo)

# Automobile companies

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



