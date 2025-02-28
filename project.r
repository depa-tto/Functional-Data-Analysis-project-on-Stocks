library(tidyquant)
library(quantmod)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(zoo)

# fashion companies

kering <- getSymbols("KER.PA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
kering <- kering$KER.PA.Close
head(kering)

cpri <- getSymbols("CPRI", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
cpri <- cpri$CPRI.Close
head(cpri)

rms <- getSymbols("RMS.PA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
rms <- rms$RMS.PA.Close
head(rms)

mc <- getSymbols("MC.PA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
mc <- mc$MC.PA.Close
head(mc)

cfr <- getSymbols("CFR.SW", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
cfr <- cfr$CFR.SW.Close
head(cfr)

ads <- getSymbols("ADS.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
ads <- ads$ADS.DE.Close
head(ads)

nke <- getSymbols("NKE.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
nke <- nke$NKE.DE.Close
head(nke)

pum <- getSymbols("PUM.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
pum <- pum$PUM.DE.Close
head(pum)

fashion_stocks <- merge(kering,cpri,rms,mc,cfr,ads,nke,pum)
head(fashion_stocks)
sum(is.na(fashion_stocks))

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

food_stoks <- merge(nestle,unilever,danone,bonduelle,pepsi,mcdonalds,kelloggs,kraft_heinz)
food_stoks <- data.frame(food_stoks)
head(food_stoks)
sum(is.na(food_stoks))

# travel 

trivago <- (getSymbols("TRVG", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE))
trivago <- trivago$TRVG.Close
head(trivago)

booking <- getSymbols("BKNG", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
booking <- booking$BKNG.Close
head(booking)

ryanair <- getSymbols("RYA.IR", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
ryanair <- ryanair$RYA.IR.Close
head(ryanair)

lyft <- getSymbols("LYFT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
lyft <- lyft$LYFT.Close
head(lyft)

trip.com <- getSymbols("TCOM", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
trip.com <- trip.com$TCOM.Close
head(trip.com)

tripadvisor <- getSymbols("TRIP", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
tripadvisor <- tripadvisor$TRIP.Close
head(tripadvisor)

hilton <- getSymbols("HLT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
hilton <- hilton$HLT.Close
head(hilton)


uber <- getSymbols("UBER", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
uber <- uber$UBER.Close
head(uber)

travel_stocks <- merge(trivago,booking,ryanair,lyft,trip.com,tripadvisor,hilton,uber)
travel_stocks <- data.frame(travel_stocks)
is.data.frame(travel_stocks)
travel_stocks$TRVG.Close


nrow(trivago)
nrow(travel_stocks)
head(trivago)
sum(is.na(trivago))
sum(is.na(travel_stocks$TRVG.Close))


which(is.na(travel_stocks$TRVG.Close))
travel_stocks[which(is.na(travel_stocks$TRVG.Close)),]
trivago[which(is.na(trivago[,'TRVG.Close'])),]

head(travel_stocks)
sum(is.na(travel_stocks))

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

chevron <- getSymbols("CHV.F", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
chevron <- chevron$CHV.F.Close
head(chevron)

repsol <- getSymbols("REP.MC", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
repsol <- repsol$REP.MC.Close
head(repsol)


total_energies <- getSymbols("TTE.PA", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
total_energies <- total_energies$TTE.PA.Close
head(total_energies)


oil_stocks <- merge(shell,eni,enel,engie,orsted,chevron,repsol,total_energies)
head(oil_stocks)
sum(is.na(oil_stocks))

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
head(dhl)


fedex <- getSymbols("FDX", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
fedex <- fedex$FDX.Close
head(fedex)
 

maersk <- getSymbols("AMKBY", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
maersk <- maersk$AMKBY.Close
head(maersk)


walmart <- getSymbols("WMT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
walmart <- walmart$WMT.Close
head(walmart)

sf_express <- getSymbols("002352.SZ", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
sf_express <- sf_express$`002352.SZ.Close`
head(sf_express)

 
logistics_stocks <- merge(zalando,ups,amazon,dhl,fedex,maersk,walmart,sf_express)
head(logistics_stocks)
sum(is.na(logistics_stocks))


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


it_stocks <- merge(spotify,netflix,nvidia,meta,apple,samsung,microsoft,google)
head(it_stocks)
sum(is.na(it_stocks))

# automobile companies

volkswagen <- getSymbols("VOW3.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
volkswagen <- volkswagen$VOW3.DE.Close
head(volkswagen)


ferrari <- getSymbols("RACE.MI", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
ferrari <- ferrari$RACE.MI.Close
head(ferrari)

stellantis <- getSymbols("STLAM.MI", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
stellantis <- stellantis$STLAM.MI.Close
head(stellantis)


renault <- getSymbols("RNO.PA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
renault <- renault$RNO.PA.Close
head(renault)


mercedes <- getSymbols("MBG.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
mercedes <- mercedes$MBG.DE.Close
head(mercedes)

BMW <- getSymbols("BMW.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
BMW <- BMW$BMW.DE.Close
head(BMW)


tesla <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
tesla <- tesla$TSLA.Close
head(tesla)


toyota <- getSymbols("TM", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
toyota <- toyota$TM.Close
head(toyota)

automobile_stocks <- merge(volkswagen,ferrari,stellantis,renault,mercedes,BMW,tesla,toyota)
head(automobile_stocks)
sum(is.na(automobile_stocks))

# healthcare companys

Sanofi <- getSymbols("SAN.PA", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Sanofi <- Sanofi$SAN.PA.Close
head(Sanofi)


Novartis <- getSymbols("NOVN.SW", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Novartis <- Novartis$NOVN.SW.Close
head(Novartis)


Bayer <- getSymbols("BAYN.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Bayer <- Bayer$BAYN.DE.Close
head(Bayer)


AstraZeneca <- getSymbols("AZN", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
AstraZeneca <- AstraZeneca$AZN.Close
head(AstraZeneca)


UCB_SA <- getSymbols("UCB.BR", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
UCB_SA <- UCB_SA$UCB.BR.Close
head(UCB_SA)


Merck_KGaA <- getSymbols("MRK.DE", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
Merck_KGaA <- Merck_KGaA$MRK.DE.Close
head(Merck_KGaA)


argenx <- getSymbols("ARGX", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
argenx <- argenx$ARGX.Close
head(argenx)


GSK <- getSymbols("GSK.L", src = "yahoo", from = "2020-01-01", to = "2022-12-31", auto.assign = FALSE)
GSK <- GSK$GSK.L.Close
head(GSK)


healthcare_stocks <- merge(Sanofi,Novartis,Bayer,AstraZeneca,UCB_SA,Merck_KGaA,argenx,GSK)
healthcare_stocks
sum(is.na(healthcare_stocks))

