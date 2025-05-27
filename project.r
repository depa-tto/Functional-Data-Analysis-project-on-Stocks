library(quantmod)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(zoo)
library(httr)

# REMOVING MISSING VALUES FUNCTION

fill_na_with_moving_avg <- function(x, window = 10) {
  na_indices <- which(is.na(x))  
  for (i in na_indices) {
    left <- max(1, i - window)   
    right <- min(length(x), i + window)  
    surrounding_values <- x[left:right]  
    if (sum(!is.na(surrounding_values)) > 0) {  
      x[i] <- mean(surrounding_values, na.rm = TRUE)  
    }
  }
  return(x)
}

st[,1:8]

# fashion companies
st
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


fashion_stocks <- as.data.frame(lapply(fashion_stocks, fill_na_with_moving_avg))
nrow(fashion_stocks) # 776
sum(is.na(fashion_stocks))

file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/fashion_stocks.csv"
# write.csv(fashion_stocks, file_path, row.names = FALSE)

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


food_stoks <- as.data.frame(lapply(food_stoks, fill_na_with_moving_avg))
nrow(food_stoks) # 776
sum(is.na(food_stoks))

file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/food_stoks.csv"
# write.csv(food_stoks, file_path, row.names = FALSE)


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


travel_stocks <- as.data.frame(lapply(travel_stocks, fill_na_with_moving_avg))
nrow(travel_stocks) # 776
sum(is.na(travel_stocks))

file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/travel_stocks.csv"
# write.csv(travel_stocks, file_path, row.names = FALSE)

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


oil_stocks <- as.data.frame(lapply(oil_stocks, fill_na_with_moving_avg))
nrow(oil_stocks) # 776
sum(is.na(oil_stocks))

file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/oil_stocks.csv"
# write.csv(oil_stocks, file_path, row.names = FALSE)

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


logistics_stocks <- as.data.frame(lapply(logistics_stocks, fill_na_with_moving_avg))
nrow(logistics_stocks) # 781
sum(is.na(logistics_stocks))

file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/logistics_stocks.csv"
# write.csv(logistics_stocks, file_path, row.names = FALSE)


# technology

spotify <- getSymbols("SPOT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
spotify <- spotify$SPOT.Close
head(spotify)
spotify[c(1,7),]

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


ibm <- getSymbols("IBM", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
ibm <- ibm$IBM.Close
head(ibm)


microsoft <- getSymbols("MSFT", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
microsoft <- microsoft$MSFT.Close
head(microsoft)

google <- getSymbols("GOOG", src = "yahoo", from="2020-01-01", to = "2022-12-31", auto.assign = FALSE)
google <- google$GOOG.Close
head(google)


it_stocks <- merge(spotify,netflix,nvidia,meta,apple,ibm,microsoft,google)
head(it_stocks)
sum(is.na(it_stocks))


it_stocks <- as.data.frame(lapply(it_stocks, fill_na_with_moving_avg))
nrow(it_stocks) # 756
sum(is.na(it_stocks))

file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/it_stocks.csv"
# write.csv(it_stocks, file_path, row.names = FALSE)


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


automobile_stocks <- as.data.frame(lapply(automobile_stocks, fill_na_with_moving_avg))
nrow(automobile_stocks) # 776
sum(is.na(automobile_stocks))


file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/automobile_stocks.csv"
# write.csv(automobile_stocks, file_path, row.names = FALSE)

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

Bayer
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


healthcare_stocks <- as.data.frame(lapply(healthcare_stocks, fill_na_with_moving_avg))
nrow(healthcare_stocks) # 776
sum(is.na(healthcare_stocks))


file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/healthcare_stocks.csv"
# write.csv(healthcare_stocks, file_path, row.names = FALSE)
library(ggplot2)

it_stocks <- data.frame(it_stocks)
automobile_stocks <- data.frame(automobile_stocks)
fashion_stocks <- data.frame(fashion_stocks)
healthcare_stocks <- data.frame(healthcare_stocks)
food_stoks <- data.frame(food_stoks)
oil_stocks <- data.frame(oil_stocks)
travel_stocks<-data.frame(travel_stocks)
logistics_stocks <- data.frame(logistics_stocks)

it_stocks$date <- as.Date(rownames(it_stocks))
automobile_stocks$date <- as.Date(rownames(automobile_stocks))
fashion_stocks$date <- as.Date(rownames(fashion_stocks))
healthcare_stocks$date <- as.Date(rownames(healthcare_stocks))
food_stoks$date <- as.Date(rownames(food_stoks))
oil_stocks$date <- as.Date(rownames(oil_stocks))
travel_stocks$date <- as.Date(rownames(travel_stocks))
logistics_stocks$date <- as.Date(rownames(logistics_stocks))

stock_list <- list(it_stocks, automobile_stocks, fashion_stocks, healthcare_stocks, 
                   food_stoks, oil_stocks, travel_stocks, logistics_stocks)

st[,17]
# Merge all datasets by 'date'

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2022-12-31")
all_stocks[3,]
# Generate all Fridays within the date range
all_dates <- seq(from = start_date, to = end_date, by = "day")

# Filter to only Fridays
fridays <- all_dates[weekdays(all_dates) == "Friday"]

all_stocks_xts <- xts(all_stocks[,-1], order.by = all_stocks$date)

# Create an empty xts object with Fridays
friday_xts <- xts(, order.by = fridays)

# Merge and fill missing values with the last observation
weekly_stocks <- merge(all_stocks_xts, friday_xts, all = TRUE)

# Fill forward and backward to handle all NAs
weekly_stocks <- na.locf(weekly_stocks, fromLast = FALSE)  # Forward fill (Thursday, Wednesday, etc.)
weekly_stocks <- na.locf(weekly_stocks, fromLast = TRUE)   # Backward fill for leading NAs

# Keep only the rows aligned to Fridays
weekly_stocks <- weekly_stocks[fridays]

# Verify the data
head(weekly_stocks)
nrow(weekly_stocks)
sum(is.na(weekly_stocks))
weekly_stocks_df <- data.frame(date = index(weekly_stocks), coredata(weekly_stocks))

getwd()
# Save the dataset
write.csv(weekly_stocks_df, "weekly_stock_prices_cleaned.csv", row.names = FALSE)

st <- weekly_stocks_df
# Check for any remaining missing values
sum(is.na(weekly_stocks_df))

# Check the result
head(weekly_stocks_df)
# Check the weekly datas
# Check the merged dataset
head(all_stocks)
dim(all_stocks)
all_stocks[,2]
colnames(all_stocks)
# your file_path

path <- getwd()
path
setwd(file.path(getwd(), "data_stocks"))

# read CSV
it <- read.csv("it_stocks.csv")
automobile <- read.csv("automobile_stocks.csv")
fashion<- read.csv("fashion_stocks.csv")
healthcare<- read.csv("healthcare_stocks.csv")
food <- read.csv("food_stoks.csv")
oil<- read.csv("oil_stocks.csv")
travel <- read.csv("travel_stocks.csv")
logistics <- read.csv("logistics_stocks.csv")
st[,1]

# merge all stocks
st <- cbind.data.frame(logistics,it,automobile,fashion,healthcare,food,oil,travel)
# save the final file
st[,17:23]
spotify
write.csv(st, file = "final_data.csv", row.names = FALSE)

########################################################################## start ur code from here ##########################################################################
library(quantmod)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(zoo)
library(httr)

# Read the file
path <- getwd()
setwd(file.path(getwd(), "data_stocks"))
st <- read.csv("weekly_stock_prices_cleaned.csv")
head(st)

sum(is.na(st))
#Data preprocessing
st <- st[, -1]
log_returns <- apply(st, 2, function(x) c(diff(log(x))))
st <- 100*log_returns
st <- data.frame(st)
dim(log_returns)
png("After_log.png", width = 1000, height = 800)
plot(st$SPOT.Close, type="l", col="blue", lwd=2, xlab="", ylab="SPOT Close", 
     main="SPOT Close Prices After Transformation", cex = 2,cex.lab = 1.8,    # Axis labels
     cex.axis = 1.8, # Axis tick labels
     cex.main = 2,   # Main title
     cex.sub = 2)
dev.off()

# Visualize stocks for IT
opar <- par(mfrow=c(2,2))
plot(st$SPOT.Close, type="l", col="blue", lwd=2, xlab="", ylab="SPOT Close", 
     main="SPOT Close Prices")
plot(st$NFLX.Close, type="l", col="red", lwd=2, xlab="", ylab="NFLX Close", 
     main="NFLX Close Prices")
plot(st$NVDA.Close, type="l", col="green", lwd=2, xlab="", ylab="NVDA Close", 
     main="NVDA Close Prices")
plot(st$META.Close, type="l", col="purple", lwd=2, xlab="", ylab="META Close", 
     main="META Close Prices")

opar <- par(mfrow=c(2,2))
plot(st$AAPL.Close, type="l", col="blue", lwd=2, xlab="", ylab="APPL Close", 
     main="APPL Close Prices")
plot(st$IBM.Close, type="l", col="red", lwd=2, xlab="", ylab="IBM Close", 
     main="IBM Close Prices")
plot(st$MSFT.Close, type="l", col="green", lwd=2, xlab="", ylab="MSFT Close", 
     main="MSFT Close Prices")
plot(st$GOOG.Close, type="l", col="purple", lwd=2, xlab="", ylab="GOOG Close", 
     main="GOOG Close Prices")

dev.off()



# Depth analysis before smoothing
install.packages("DepthProc")
install.packages("MultiRNG")
library(DepthProc)
library(MultiRNG)

st <- as.matrix(st)
sum(is.na(st))

# Euclidean depth

dE <- depthEuclid(st, st)
mdE <- which.max(dE)
dE[mdE] # euclidean depth of the deepest point in the dataset

# a very small depth value (like this one) suggests that the dataset 
# is highly spread out or has strong outliers, meaning even the "deepest" point is not very central.

st[mdE,] # retrieves the most central (deepest) row from the dataset based on Euclidean Depth


apply(st,2,median)

plot(st)
points(st[mdE,1], st[mdE,2], pch=23, col="blue", bg="blue", lwd=2)
points(median(st[,1]), median(st[,2]), pch=24, col="red", bg="red", lwd=2)
# red one is median point while the blue one is the deepest point, affected by outliers
# if the blue (deepest) and red (median) points are close, the dataset is relatively symmetric and balanced.
# if they are far apart, the dataset might have outliers or skewed distribution (the Euclidean Depth is influenced by the overall spread of the data)


# Local depth
dL <- depthLocal(st, st, depth_params1 = list(method = "LP"))
dL
mdL <- which.max(dL)
dL[mdL]
st[mdL,]

depthContour(st[,1:2], depth_params = list(method = "Local", depth_params1 = list(method = "LP")))

# MBD, Frainman-Muniz
dMBD <- fncDepth(st, method = "MBD")
dFM <- fncDepth(st, method = "FM")
mdMBD <- which.max(dMBD)
mdFM <- which.max(dFM)

dMBD[mdMBD]
st[mdMBD,]

dFM[mdFM]
st[mdFM,]
png("depth.png", width = 1000, height = 800)
matplot(t(st), type = "l", lty = 1, col = "gray",
        xlab = "Week", ylab = "Stock Value",
        main = "Stock Curves with Most Central (FM) Highlighted")

# Overlay the most central stock's curve in red
lines(st[mdFM, ], col = "red", lwd = 2)
dev.off()
plot(st)
points(st[mdMBD,5], st[mdMBD,6], pch=23, col="blue", bg="blue", lwd=2)
points(st[mdFM,5], st[mdFM,6], pch=23, col="green", bg="green", lwd=2)
points(median(st[,5]), median(st[,6]), pch=24, col="red", bg="red", lwd=2)

png("Friman.png", width = 800, height = 600)

# Plot the data in st (this will use the default plot method for a matrix)
plot(st, main = "Stock Data with Depth", cex.main = 2)

# Add a blue point for the most central stock (by MBD) using columns 3 and 4.
points(st[mdFM,5], st[mdFM,6], pch=23, col="blue", bg="blue", lwd=2)

# Add a red point for the coordinate-wise median of columns 3 and 4.
points(median(st[, 3]), median(st[, 4]), pch = 24, col = "red", bg = "red", lwd = 2)

# Add a legend to the top right corner
legend("topright", 
       legend = c("Friman-Munith depth", "Median"), 
       pch = c(23, 24), 
       col = c("blue", "red"), 
       pt.bg = c("blue", "red"), 
       lwd = 2,cex = 1.6)

# Close the device to save the file.
dev.off()
fncDepthMedian(st, method = "MBD")
fncDepthMedian(st, method = "FM")
getwd()
# B-splines with penalty
library(fda)
library(fda.usc)

### B-splines
dim(st)
st <- as.matrix(st)
day <- c(1:156)

nrow(st)
# Create a grid for lambda and number of basis
l <- c(0 ,2^seq(-9, 9, len = 40))
nb <- seq(7, 40, by = 2)
time_points <- 1:156
# Create functional ojects with argumen values
fdata_obj <- fdata(t(st), argvals = time_points)


# Smooth with B-splines
out0 <- optim.basis(fdata_obj, lambda = l, numbasis = nb, type.basis = "bspline")
sum((fdata_obj$data - out0$fdata.est)^2)
basis <- create.bspline.basis(c(1,156),nbasis= out0$numbasis.opt, norder = 4)

out0$fdata.est # the smoothed functional data
out0$numbasis.opt # the optimal number of basis functions

# Calculate SSE
SSE <-sum((fdata_obj - out0$fdata.est )^2)


gcv = rep(0,40)
df = rep(0,40)
sse = rep(0,40)

# Iterate with different lambda for graph
lambda_seq = c(0 ,2^seq(-9, 9, len = 40))
for(i in 1:40){
  lambda=lambda_seq[i]
  tD2fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=lambda)
  
  smooth = smooth.basis(day, st, tD2fdPar)
  
  gcv[i] = sum(smooth$gcv)
  df[i] = smooth$df
  sse[i] = smooth$SSE
}

# Plot df, SSE and GCV
par(mfrow = c(3,1))
plot(0:39,df[1:40],type='l',xlab='log lambda',ylab='df',cex.lab=1.5)
plot(0:39,sse[1:40],type='l',xlab='log lambda',ylab='sse',cex.lab=1.5)
plot(0:39,gcv[1:40],type='l',xlab='log lambda',ylab='gcv',cex.lab=1.5)
dev.off()
getwd()
#Download the plot
png("sse_plot.png", width = 800, height = 600)

plot(seq(-9, 9, len = 40), sse[1:40], type = 'l',
     xlab = 'log(lambda)', ylab = 'SSE',
     cex.lab = 1.5, lwd = 2, col = 'blue',
     main = 'SSE vs. log(lambda)', cex.main = 1.8)
grid()
dev.off()

png("gcv_plot.png", width = 800, height = 600)


plot(seq(-9, 9, len = 40), gcv[1:40], type = 'l',
     xlab = 'log(lambda)', ylab = 'GCV',
     cex.lab = 1.5, lwd = 2, col = 'red',
     main = 'GCV vs. log(lambda)', cex.main = 1.8)
grid()

dev.off()


# Find optimal lambda
optimal_lambda_index = which.min(gcv)
optimal_lambda = lambda_seq[optimal_lambda_index]
optimal_df = df[optimal_lambda_index]
optimal_sse = sse[optimal_lambda_index]
basis <- create.bspline.basis(c(1,156),nbasis= 7, norder = 4)


tD3fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=optimal_lambda)
smooth <- smooth.basis(day,st,tD3fdPar)
smooth$SSE
plot(smooth$fd)
par()
dev.off()
fitted<- eval.fd(1:156, smooth$fd)
plot(st[,3])
lines(fitted[,3])

<<<<<<< HEAD
<<<<<<< HEAD

plot(out0$fdataobj)
names(out0$fdataobj)
dim(t(out0$fdataobj$data))
dim(t(st))
SSE <-sum((st - t(out0$fdataobj$data))^2)
st[1,]

png("B-splines.png",width = 1000, height = 800)
plot(smooth$fd, lwd = 2,main = 'B-splines fiited', cex = 2,cex.lab = 1.8,    # Axis labels
     cex.axis = 1.8, # Axis tick labels
     cex.main = 2,   # Main title
     cex.sub = 2)
dev.off()


<<<<<<< HEAD
=======


>>>>>>> parent of 7cf9c51 (Kernel updated)
=======
>>>>>>> 70544f7541eccaec1306643b96303d22992ef2d8
#Kernel smoothing
=======

# EDA and outliers detection for b-spline
smooth.fd = smooth$fd
dev.off()
plot(smooth.fd)
D1 <- deriv.fd(smooth.fd, deriv = 1)
D2 <- deriv.fd(smooth.fd, deriv = 2)
plot(D1)
plot(D2)
png("first_derivative.png", width = 1200, height = 800)

plot(D1, , lwd = 2, main = "First Derivative",
     xlab = "Time", ylab = "First Derivative Value",cex = 2,cex.lab = 1.8,    # Axis labels
     cex.axis = 1.8, # Axis tick labels
     cex.main = 2,   # Main title
     cex.sub = 2)

legend("topright", legend = "First Derivative", col = "blue", lwd = 2)

dev.off()  # Close the device to save the plot

# 2. Save Second Derivative Plot
png("second_derivative.png", width = 1200, height = 800)

plot(D2, lwd = 2, main = "Second Derivative",
     xlab = "Time", ylab = "Second Derivative Value",cex = 2,cex.lab = 1.8,    # Axis labels
     cex.axis = 1.8, # Axis tick labels
     cex.main = 2,   # Main title
     cex.sub = 2)


dev.off()  # Close the device to save the plot
b_spline_mean = mean.fd(smooth.fd)
b_spline_sd = std.fd(smooth.fd)
plot(smooth.fd)
b_spline_mean = mean.fd(smooth.fd)
lines(b_spline_mean, lwd=3, lty=2, col=2)
b_spline_mean <- mean.fd(smooth.fd)

# Open a graphics device to save the plot (e.g., PNG)
png("functional_data_plot_large_text.png", width = 1200, height = 900)

# Plot the smooth.fd object with larger text sizes
plot(smooth.fd, main = "Functional Data with Mean Curve", 
     cex.main = 3,      # Title text size
     cex.lab = 2.5,     # Axis labels size
     cex.axis = 2)      # Axis tick labels size

# Add the mean curve with specified line width, type, and color
lines(b_spline_mean, lwd = 5, lty = 2, col = 2)

# Add a legend with larger text
legend("topright", legend = c("Smooth Data", "Mean Curve"),
       col = c(1, 2), lty = c(1, 2), lwd = c(3, 5),
       cex = 2) # Legend text size

# Close the graphics device
dev.off()

lines(b_spline_mean-b_spline_sd, lwd=3, lty=2, col=6)
lines(b_spline_mean+b_spline_sd, lwd=3, lty=2, col=6)
dev.off()



# the Bivariate Covariance Function v(s; t)

logprecvar.bifd = var.fd(smooth.fd)

print(range(logprecvar.bifd$argvals))

time = seq(1,156, length = 36)
logprecvar_mat = eval.bifd(weektime, weektime,
                            logprecvar.bifd)

persp(weektime, weektime, logprecvar_mat,
      theta=-20, phi=20, r=3, expand = 0.5,
      ticktype='detailed',
      xlab="Week",
      ylab="Week",
      zlab="variance")
png("Variance_perspective.png", width = 1200, height = 1000)

# Create the perspective plot
persp(weektime, weektime, logprecvar_mat,
      theta = -20, phi = 20, r = 3, expand = 0.5,
      ticktype = 'detailed',
      xlab = "Week",
      ylab = "Week",
      zlab = "variance",labcex=2)

# Close the device to finalize and save the file
dev.off()

contour(weektime, weektime, logprecvar_mat,
        xlab="Day",
        ylab="Day")

png("Coontour.png", width = 1200, height = 1000)
day5time = seq(1,156)
logprec.varmat = eval.bifd(day5time, day5time,
                           logprecvar.bifd)
contour(day5time, day5time, logprec.varmat,
        xlab="Day",
        ylab="Day", lwd=2.2,
        labcex=2)
dev.off()

### Descriptive measures for functional data.

library(fda.usc)
data(poblenou)
nox <- poblenou$nox
working <- poblenou$nox[poblenou$df$day.festive == 0 &
                          as.integer(poblenou$df$day.week) < 6]
nonworking <- poblenou$nox[poblenou$df$day.festive == 1 |
                             as.integer(poblenou$df$day.week) > 5]

# Centrality measures (working)

par( mfrow=c(2, 2) )
plot(func.mean(working), ylim = c(10, 170),
     main = "Centrality measures in working days")
legend(x = 11, y = 170, cex = 1, box.col = "white", lty = 1:5,
       col = c(1:5), legend = c("mean","trim.mode","trim.RP",
                                "median.mode","median.RP"))
lines(func.trim.mode(working, trim = 0.15), col = 2, lty = 2)
lines(func.trim.RP(working, trim = 0.15), col = 3, lty = 3)
lines(func.med.mode(working, trim = 0.15), col = 4, lty = 4)
lines(func.med.RP(working, trim = 0.15), col = 5, lty = 5)

# Centrality measures (non-working)
plot(func.mean(nonworking), ylim = c(10,170),
     main = "Centrality measures in non-working days")
legend(x = 11, y = 170, cex = 1, box.col = "white",lty = 1:5,
       col = c(1:5), legend = c("mean","trim.mode","trim.RP",
                                "median.mode","median.RP"))
lines(func.trim.mode(nonworking, trim = 0.15),col = 2, lty = 2)
lines(func.trim.RP(nonworking, trim = 0.15),col = 3, lty = 3)
lines(func.med.mode(nonworking, trim = 0.15),col = 4, lty = 4)
lines(func.med.RP(nonworking, trim = 0.15),col = 5, lty = 5)

# Measures of dispersion   (working)
plot(func.var(working),
     main = "Dispersion measures in working days", ylim = c(100 ,5500))
legend(x = 11, y = 5300,cex = 1, box.col = "white", lty = 1:3, col = 1:3,
       legend = c("var", "trimvar.mode", "trimvar.RP"))
lines(func.trimvar.mode(working,trim = 0.15), col = 2, lty = 2)
lines(func.trimvar.RP(working,trim = 0.15), col = 3, lty = 3)

# Measures of dispersion   (non-working)
plot(func.var(nonworking),
     main = "Dispersion measures in non-working days", ylim = c(100, 5500))
legend(x = 11, y = 5300, cex = 1, box.col = "white", lty = 1:3, col = 1:3,
       legend = c("var", "trimvar.mode", "trimvar.RP"))
lines(func.trimvar.mode(nonworking, trim = 0.15), col = 2, lty = 2)
lines(func.trimvar.RP(nonworking, trim = 0.15), col = 3, lty = 3)

dev.off()

### boxplot

boxplot(smooth.fd)


### Kernel smoothing
>>>>>>> 1ff0d3075724ddecfd65ff8446f38908e4efff95
out1 <- optim.np(fdata_obj , type.S = S.NW, h = 3:50,par.CV = list(criteria = "GCV"))#Local regression
out2 <- optim.np(fdata_obj, type.S = S.LLR,h = 3:50, par.CV = list(criteria = "GCV"))#Local kernel

out3 <- optim.np(fdata_obj, type.S = S.KNN, h = 3:50, Ker = Ker.norm) # Normal Kernel
out4 <- optim.np(fdata_obj, type.S = S.NW, h = 3:50, Ker = Ker.tri, correl = FALSE) #Triweight Kernel
out5 <- optim.np(fdata_obj, type.S = S.NW, h = 3:50, Ker = Ker.epa, correl = FALSE) #Epanechnikov Kerne
out6 <- optim.np(fdata_obj, type.S = S.NW, h = 3:50, Ker = Ker.unif, correl = FALSE) #Uniform Kernel

SSE_out0 <-sum((fdata_obj - out0$fdata.est )^2)
SSE_out1 <-sum((fdata_obj - out1$fdata.est )^2)
SSE_out2 <-sum((fdata_obj - out2$fdata.est )^2)
SSE_out3 <-sum((fdata_obj - out3$fdata.est )^2)
SSE_out4 <-sum((fdata_obj - out4$fdata.est )^2)
SSE_out5 <-sum((fdata_obj - out5$fdata.est )^2)
SSE_out6 <-sum((fdata_obj - out6$fdata.est )^2)


# Combine GCV values into a vector
gcv_values <- c(out0$gcv.opt, out1$gcv.opt, out2$gcv.opt, out3$gcv.opt, 
                out4$gcv.opt, out5$gcv.opt, out6$gcv.opt)

# Define labels for each method
methods <- c("B-splines", "NW", "LR", "Normal ", "Triweight ", "Epanech.", "Uniform")

# Plot GCV values


png("GCV_comparison_plot.png", width = 1200, height = 800)

par(mar = c(5, 7, 4, 2) + 0.1)  # Adjust margins for better text spacing

# Create the barplot without rotating labels
bp <- barplot(gcv_values, names.arg = methods, col = "skyblue", 
              main = "GCV Comparison by smoothing methods", 
              ylab = "GCV Value", cex.names = 2, cex.lab = 2, cex.main = 3, 
              ylim = c(0, max(gcv_values) * 1.1), border = "white")

# Add horizontal grid lines for clarity
grid(nx = NULL, ny = NULL, col = "gray90", lty = 2)


dev.off()  

# Compute Sum of Squared Errors (SSE) for each method
sse_values <- c(SSE_out0, SSE_out1, SSE_out2, SSE_out3, SSE_out4, SSE_out5, SSE_out6)

# Plot SSE values
png("SSE_comparison_plot.png", width = 1200, height = 800)

sse_values_adjusted <- sse_values[-1]  # Adjust SSE values if removing the first method
methods_adjusted <- methods[-1]  # Adjust method names to match
options(scipen = 999)
# Improved plot for SSE values
png("SSE_comparison_plot.png", width = 1200, height = 800)

par(mar = c(5, 7, 4, 2) + 0.1)  # Adjust margins

# Create the barplot without rotating labels
bp_sse <- barplot(sse_values, names.arg = methods, col = "lightcoral", 
                  main = "SSE Comparison by smoothing methods", 
                  ylab = "Sum of Squared Errors", 
                  cex.names = 2, cex.lab = 2, cex.main = 3, 
                  ylim = c(0, max(sse_values) * 1.1), border = "white")

# Add horizontal grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray90", lty = 2)


dev.off()  # Save the plot to file

png("gcv_criteria_plot.png", width = 1200, height = 800)

# Set plot margins and background for better visuals
par(mar = c(5, 6, 4, 8) + 0.1, bg = "white")  # Increased right margin for space

# Main plot with better aesthetics
plot(out1$h, out1$gcv, type = "l",
     main = "GCV Criteria",
     xlab = "Bandwidth (h) Values",
     ylab = "GCV Criteria",
     col = "forestgreen", lwd = 3,
     cex.lab = 1.8, cex.main = 2, cex.axis = 1.5)

# Add grid lines for better clarity
grid(lty = "dotted", col = "gray70")

# Add additional lines with unique colors and styles
lines(out2$h, out2$gcv, col = "royalblue", lwd = 3, lty = 2)
lines(out3$h, out3$gcv, col = "darkorange", lwd = 3, lty = 3)
lines(out4$h, out4$gcv, col = "purple", lwd = 3, lty = 4)
lines(out5$h, out5$gcv, col = "firebrick", lwd = 3, lty = 5)
lines(out6$h, out6$gcv, col = "goldenrod", lwd = 3, lty = 6)

# Improved legend with larger text and custom position (left side)
legend("topright", inset = c(0.01, 0),
       legend = c("Ker.norm-S.NW", "Ker.norm-S.LLR", 
                  "Ker.norm-S.KNN", "Ker.tri-S.NW",
                  "Ker.epa-S.NW", "Ker.unif-S.NW"),
       col = c("forestgreen", "royalblue", "darkorange", 
               "purple", "firebrick", "goldenrod"),
       lwd = 3, lty = 1:6,
       box.col = "white", cex = 2)

# Close the graphics device to save the image
dev.off()


plot(SSE_out1)

names(out1)

contour(nb, l, out0$gcv, ylab = "Lambda", xlab = "Number of basis", 
        main = "GCV criteria by optim.basis()")


dev.new(width = 150, height = 110, units = "mm")
plot(out1$h, out1$gcv, type = "l", main = "GCV criteria  by optim.np() ", 
     xlab = "Bandwidth (h) values",ylab = "GCV criteria", col = 3, lwd = 2)
legend(x = 3, y = 6, legend = c("Ker.norm-S.NW", "Ker.norm-S.LLR", 
                                  "Ker.norm-S.KNN", "Ker.tri-S.NW",
                                  "Ker.epa-S.NW", "Ker.unif-S.NW"),
       box.col = "white", lwd = c(2, 2, 2), col = c(3, 4, 5, 6, 7, 8),cex = 0.75)
lines(out3$h,out3$gcv, col = 5, lwd = 2)
lines(out4$h,out4$gcv, col = 6, lwd = 2)
lines(out5$h,out5$gcv, col = 7, lwd = 2)
lines(out6$h,out6$gcv, col = 8, lwd = 2)

plot(out2$h, out2$gcv, type = "l", main = "GCV criteria  by optim.np() ", 
     xlab = "Bandwidth (h) values",ylab = "GCV criteria", col = 3, lwd = 2)

###Plotting the differet smoothing 
lines(st[,1], col = "red")
par(mfrow = c(1,2))
plot(out0$fdata.est[1,],col ="blue", lwd = 3)
points(st[,4], col = "red")

plot(out3$fdata.est[4,],col ="blue", lwd = 3)
points(st[,4], col = "red")
par(opar)
dev.off()

png("B-splines fitted.png", width = 1200, height = 800)
plot(out0$fdata.est[2,], col = "blue", lwd = 3, 
     main = "B-splines fitted", 
     xlab = "Time", ylab = "Price", 
     cex.main = 3, cex.lab = 2, cex.axis = 2)

# Add red points for the second data
points(st[, 2], col = "red", cex = 2)
dev.off()
png("Normal kernel fitted.png", width = 1200, height = 800)
plot(out3$fdata.est[1,], col = "blue", lwd = 3, 
     main = "Normal kernel fitted", 
     xlab = "Time", ylab = "Price", 
     cex.main = 3, cex.lab = 2, cex.axis = 2)

# Add red points for the second data
points(st[, 2], col = "red", cex = 2)
dev.off()

png("Triweight kernel fitted.png", width = 1200, height = 800)
plot(out4$fdata.est[2,], col = "blue", lwd = 3, 
     main = "Triweight kernel fitted", 
     xlab = "Time", ylab = "Price", 
     cex.main = 3, cex.lab = 2, cex.axis = 2)

# Add red points for the second data
points(st[, 2], col = "red", cex = 2)
dev.off()


### Selected smoothing ###



#PCA
library(fda)
nharm = 4
pcalist = pca.fd(smooth$fd, nharm, centerfns = TRUE)
plot(pcalist)
plot(pcalist$harmonics)

for (i in 1:6) {
  png(paste0("pca_plot_", i, ".png"), width = 800, height = 600)
  plot(pcalist, harm = i, cex = 2,cex.lab = 2,    # Axis labels
       cex.axis = 1.5, # Axis tick labels
       cex.main = 2,   # Main title
       cex.sub = 1.5)  # 'harm' specifies which component to plot
  dev.off()
}

dev.off()
plotscores(pcalist, loc = 5)

fd.pca1.list <- list() 
fd.pca2.list <- list() 
fd.pca3.list <- list() 
fd.pca4.list <- list() 


for(i in 1:5) {
  fd.pca1.list[[i]] <- mean.fd(smooth$fd) + 
    pcalist$scores[i,1]*pcalist$harmonics[1]
  
  fd.pca2.list[[i]] <- mean.fd(smooth$fd) + 
    pcalist$scores[i,1]*pcalist$harmonics[1] + 
    pcalist$scores[i,2]*pcalist$harmonics[2]
  
  fd.pca3.list[[i]]<- mean.fd(smooth$fd) +
    pcalist$scores[i,1]*pcalist$harmonics[1] + 
    pcalist$scores[i,2]*pcalist$harmonics[2] +
    pcalist$scores[i,3]*pcalist$harmonics[3] 
  
  fd.pca4.list[[i]]<- mean.fd(smooth$fd) +
    pcalist$scores[i,1]*pcalist$harmonics[1] + 
    pcalist$scores[i,2]*pcalist$harmonics[2] +
    pcalist$scores[i,3]*pcalist$harmonics[3] +
    pcalist$scores[i,4]*pcalist$harmonics[4]
}
plot(mean.fd(smooth$fd) + pcalist$scores[1,1]*pcalist$harmonics[1])
plot(mean.fd(smooth$fd))

opar <- par(mfrow=c(2,2), ask = TRUE)
for(i in 1:5) {
  plot(fd.pca1.list[[i]], ylim=c(-1, 1), ylab = "1 PC")
  lines(smooth$fd[i], col = 2)
  
  plot(fd.pca2.list[[i]], ylim=c(-1, 1), ylab = "2 PC")
  lines(smooth$fd[i], col = 2)
  
  plot(fd.pca3.list[[i]], ylim=c(-1, 1), ylab = "3 PC")
  lines(smooth$fd[i], col = 2)
  
  plot(fd.pca4.list[[i]], ylim=c(-1, 1), ylab = "4 PC")
  lines(smooth$fd[i], col = 2)
}
par(opar)
opar <- par(mfrow=c(2,2), ask = TRUE)
plot(fd.pca1.list[[1]])
lines(smooth$fd[1], col = 2)

plot(fd.pca2.list[[1]])
lines(smooth$fd[1])
     
varmx <- varmx.pca.fd(pcalist)
plot(varmx)
par(mfrow= c(2,3))
for (i in 1:6) {
  png(paste0("varimx_plot_", i, ".png"), width = 1000, height = 800)
  plot(varmx, harm = i, cex = 2,cex.lab = 2,    # Axis labels
       cex.axis = 1.5, # Axis tick labels
       cex.main = 2,   # Main title
       cex.sub = 1.5)
  dev.off()
}

plot(varmx$harmonics)

png("harmonics_plot.png")  # Save as PNG
plot(varmx$harmonics)
dev.off()
plotscores(varmx, loc = 5)

fd.vrm1.list <- list() 
fd.vrm2.list <- list() 
fd.vrm3.list <- list() 
fd.vrm4.list <- list() 


for(i in 1:5) {
  fd.vrm1.list[[i]] <- mean.fd(smooth$fd) + 
    varmx$scores[i,1]*varmx$harmonics[1]
  
  fd.vrm2.list[[i]] <- mean.fd(smooth$fd) +
    varmx$scores[i,1]*varmx$harmonics[1] + 
    varmx$scores[i,2]*varmx$harmonics[2]
  
  fd.vrm3.list[[i]]<- mean.fd(smooth$fd) +
    varmx$scores[i,1]*varmx$harmonics[1] + 
    varmx$scores[i,2]*varmx$harmonics[2] +
    varmx$scores[i,3]*varmx$harmonics[3] 
}

opar <- par(mfrow=c(2,2), ask = TRUE)
for(i in 1:5) {
  plot(fd.vrm1.list[[i]], ylim=c(-1, 1), ylab = "1 PC")
  lines(smooth$fd[i], col = 2)
  
  plot(fd.vrm2.list[[i]], ylim=c(-1, 1), ylab = "2 PC")
  lines(smooth$fd[i], col = 2)
  
  plot(fd.vrm3.list[[i]], ylim=c(-1, 1), ylab = "3 PC")
  lines(smooth$fd[i], col = 2)
}
par(opar)

for (i in 1:5) {
  # Set the output to a PNG file (you can also use pdf(), jpeg(), etc.)
  png(paste0("reconstruction_", i, ".png"), width = 1200, height = 900)
  
  # Save 2x2 grid of plots
  par(mfrow = c(2, 2)) 
  
  # 1 PC Reconstruction
  plot(fd.vrm1.list[[i]], ylim = c(-1, 1), ylab = "1 PC")
  lines(smooth$fd[i], col = 2)
  
  # 2 PC Reconstruction
  plot(fd.vrm2.list[[i]], ylim = c(-1, 1), ylab = "2 PC")
  lines(smooth$fd[i], col = 2)
  
  # 3 PC Reconstruction
  plot(fd.vrm3.list[[i]], ylim = c(-1, 1), ylab = "3 PC")
  lines(smooth$fd[i], col = 2)
  
  # Close the current image file
  dev.off()
}




<<<<<<< HEAD
lines(b_spline_mean-2*b_spline_sd, lwd=4, lty=2, col=8)
lines(b_spline_mean+2*b_spline_sd, lwd=4, lty=2, col=8)

=======
>>>>>>> 1ff0d3075724ddecfd65ff8446f38908e4efff95

basis <- create.bspline.basis(c(1,156),nbasis= out0$numbasis.opt, norder = 4)
tD3fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=out0$lambda.opt)
smooth <- smooth.basis(day,st,tD3fdPar)
mean(smooth$y[,1:8])
mean(smooth$y[9:18])

time_grid <- 1:156

original_names <- c(
  "it_stocks", "automobile_stocks", "fashion_stocks", "healthcare_stocks",
  "food_stoks", "oil_stocks", "travel_stocks", "logistics_stocks"
)

# Simplified names (same order)
short_names <- c(
  "IT", "Auto", "Fashion", "Health",
  "Food", "Oil", "Travel", "Logistics"
)

groups <- factor(rep(short_names, each = 8))
head(groups, 16)
dim(fdata_stocks)

fANOVA.pointwise <- function(data, groups, t.seq = NULL, alpha = 0.05) {
  # Check inputs
  if (length(groups) != ncol(data)) 
    stop("Length of 'groups' must match number of columns in 'data'.")
  if (!is.factor(groups)) 
    groups <- factor(groups)}
library(dplyr)

n_time <- nrow(st)
n_groups <- length(levels(groups))
group_levels <- levels(groups)
pvals <- numeric(n_time)
mean_vals <- matrix(NA, nrow = n_time, ncol = length(levels(groups)))
colnames(mean_vals) <- levels(groups)

combs <- combn(group_levels, 2)
perm <- ncol(combs)
pvals <- numeric(n_time)

Tukey.posthoc <- matrix(NA, nrow = n_time, ncol = perm)
colnames(Tukey.posthoc) <- apply(combs, 2, paste, collapse = " vs. ")
data <- eval.fd(1:156, smooth$fd)

for (i in 1:n_time) {
  # Fit ANOVA for week i
  dt <- data.frame(
    Price = data[i, ],  # Stock prices at week i
    Industry = groups   # Industry labels (IT, Auto, ...)
  )
  av <- aov(Price ~ Industry, data = dt)
  
  # Extract ANOVA p-value
  pvals[i] <- summary(av)[[1]]$`Pr(>F)`[1]
  
  # Calculate industry means for week i
  mean_vals[i, ] <- tapply(dt$Price, dt$Industry, mean)
  
  # Tukey HSD post-hoc test
  tukey_res <- TukeyHSD(av)$Industry[, "p adj"]
  Tukey.posthoc[i, ] <- tukey_res
}
sig_level <- 0.05
alpha <- 0.05
t.seq <- 1:156
# Plot p-values with significance highlights
png("ANOVA_1.png", width = 1200, height = 800, res = 150)
plot(1:156, pvals, type = "l", lwd = 2, col = "darkred",
     main = "Pointwise ANOVA",
     xlab = "Time (weeks)", ylab = "p-value", ylim = c(0, 1),
     cex.lab = 1.8,     # axis label size
     cex.axis = 1.5,    # tick number size
     cex.main = 2)
abline(h = alpha, col = "blue", lty = 2, lwd = 2)

pvals_adj <- p.adjust(pvals, method = "fdr")
# Highlight significant weeks (FDR-adjusted)
sig_weeks <- which(pvals_adj < alpha)
points(t.seq[sig_weeks], pvals[sig_weeks], col = "red", pch = 19, cex = 0.6)
legend("topright", legend = c("p-values", "Statistically significant "), 
       col = c("darkred", "red"), lwd = c(2, NA), pch = c(NA, 19), bty = "n")
dev.off()
overall_mean <- rowMeans(data)
col_set <- rainbow(n_groups)
ylim_range <- range(c(mean.p, overall_mean), na.rm = TRUE) * c(0.95, 1.05)

png("industry_mean_plot.png", width = 1200, height = 800, res = 150)
industry_means <- t(sapply(1:nrow(data), function(i) {
  tapply(data[i, ], groups, mean, na.rm = TRUE)
}))

plot(1:156, industry_means[, 1], type = "n", 
     ylim = range(industry_means, na.rm = TRUE),
     xlab = "Week", ylab = "Mean Stock Price", 
     main = "Industry Mean Trajectories",
     cex.lab = 1.8,     # axis label size
     cex.axis = 1.5,    # tick number size
     cex.main = 2 )

col_set <- rainbow(ncol(industry_means))  # Color palette
for (i in 1:ncol(industry_means)) {
  lines(1:156, industry_means[, i], col = col_set[i], lwd = 2)
}

# Add legend
legend("bottomright", legend = colnames(industry_means), 
       col = col_set, lwd = 2, cex = 0.9)
dev.off()

opar2 <- par(mfrow = c(1, 1), ask = TRUE)
for (i in 1:perm) {
  plot(t.seq, Tukey.posthoc[, i], type = "l", col = "purple", lwd = 2,
       main = paste("Tukey HSD p-values for", colnames(Tukey.posthoc)[i]),
       xlab = "Time", ylab = "p-value", ylim = c(0, 1))
  abline(h = alpha, col = "blue", lty = 2, lwd = 2)
}
par(opar2)
## --- Return --- ##
install.packages("funFEM")
library(funFEM)

res_v = funFEM(smooth$fd,K=5,model="AkjBk",init="kmeans",lambda=0,disp=TRUE)

plot(t(smooth$fd$coefs) %*% res_v$U,col=res_v$cls,pch=19,main="Discriminative space")
text(t(smooth$fd$coefs) %*% res_v$U)

data <- eval.fd(1:156, smooth$fd)

group.label <- factor(res_v$cls)

fANOVA.pointwise(data=data, groups=group.label,
                 t.seq=tsq, alpha=0.05)


group_means <- t(apply(data, 1, function(x) {
  tapply(x, group.label, mean)
}))

colors <- rainbow(length(levels(group.label)))

# Plot all group means
n.groups <- length(levels(group.label))

# Colors for each group
colors <- rainbow(n.groups)  # or choose manually

# Now plot
matplot(1:156, group_means, type = "l", lty = 1, col = colors,
        xlab = "Time", ylab = "Mean Value", main = "Mean Curves per Group")
legend("topright", legend = paste("Group", levels(group.label)),
       col = colors, lty = 1, cex = 0.8)

p.values <- sapply(1:156, function(i) {
  summary(aov(data[i, ] ~ group.label))[[1]][["Pr(>F)"]][1]
})

plot(1:156, p.values, type = "l", lwd = 2, col = "blue",
     xlab = "Time", ylab = "P-value", main = "Pointwise ANOVA p-values")
abline(h = 0.05, col = "red", lty = 2)  

install.packages("fdANOVA")
library(fdANOVA)

plotFANOVA(x = data, 
           group.label = group.label, 
           int = c(0.025, 0.975), 
           means = TRUE)
plotFANOVA(x = data, 
           group.label = group.label, 
           int = c(0.025, 0.975))

plotFANOVA(x = data, group.label = group.label,
           int = c(0.025, 0.975),separately = TRUE)
#_________________________Funnctional Autoregressive process______________
library(fda)
library(far)
basis <- create.bspline.basis(c(1,156),nbasis= out0$numbasis.opt, norder = 4)
tD3fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=out0$lambda.opt)
smooth <- smooth.basis(day,st,tD3fdPar)

# Transpose


time_grid <- 1:156

eval_matrix <- t(eval.fd(1:156, smooth$fd))
fdata_obj[1, ]$var
fdata_obj <- far::as.fdata(eval_matrix,argvals = 1:64)
str(fdata_obj$var)
dim(fdata_obj$var)
train_weeks <- 1:146
test_weeks <- 147:156
fdata_train <- fdata_obj$var[, train_weeks]
fdata_train <- far::as.fdata(fdata_train,argvals = 1:64)
fdata_test <- fdata_obj$var[, test_weeks]
fdata_test <- far::as.fdata(fdata_test,argvals = 1:64)
far::multplot(fdata_obj$"var", type = "l")
# Fit FAR(1)

model1 <- far(data=fdata_train, y="var", center=TRUE,na.rm=FALSE,kn=4)
print(model1)

print(model1)
pred_far1 <- predict(model1, newdata=fdata_train, na.rm=FALSE)

pred1 <-predict(model1, newdata=fdata_test, na.rm=FALSE)

actual_fd <- select.fdata(fdata_test)
error_fd <- actual_fd[[1]] - pred1[[1]]

png("pred_test_set.png", width = 1000, height = 800)

far::multplot(
  pred1$"var", 
  type = "l",
  main = "Predicted on Test set",
  xlab = "Stock Index",
  ylab = "Week",
  cex.lab = 2,
  cex.main = 3
)

dev.off()

png("predited_plot.png", width = 1000, height = 800)

far::multplot(
  pred_far1$"var", 
  type = "l",
  main = "Fitted values",
  xlab = "Stock Index",
  ylab = "Week",
  cex.main = 3,     # Increases main title size
  cex.lab = 2,    # Increases axis label size
  cex.axis = 1.5
)


dev.off()

png("errors.png", width = 1000, height = 800)

far::multplot(
  error_fd, 
  type = "l",
  main = "Residuals",
  xlab = "Stock Index",
  ylab = "Residuals",
  cex.lab = 2,
  cex.main = 3
)


dev.off()

model2.cv <- far.cv(data=fdata_train, y="var",ncv=40,
                    cvcrit = "var",
                    center=TRUE, na.rm = FALSE)
print(model2.cv)
k2 <- model2.cv$minL2[1]

error_mat <- unclass(as.fdata(error_fd))$var  # extract error values matrix (64 x 155)
actual_mat <- unclass(as.fdata(actual_fd[[1]]))$var 

mse <- mean(error_mat^2)

# MAPE: average absolute percentage error (x100%) over all stocks and weeks
mape <- mean(abs(error_mat / actual_mat)) * 100

ise_per_week <- colSums(error_mat^2)
mean_ise <- mean(ise_per_week)

matplot(actual_fd, type = "l")

dev.off()
plot(rowMeans(eval.fd(fdobj)))
#_________________________lag____________________
fd_input <- t(eval_matrix)
dim()
fd_centered <- scale(fd_input, center = TRUE, scale = FALSE)  # Don't scale
X_t  <- fd_centered[5:156, ]     # 154 x 64
X_t2 <- fd_centered[1:152, ]
Gamma2 <- t(X_t) %*% X_t2 / (nrow(X_t) - 1)  # 64 x 64 matrix

# 3D surface plot (optional)
grid <- seq(0, 1, length.out = 64)
z <- Gamma1

# Create a color palette
nbcol <- 100  # Number of colors
color_palette <- heat.colors(nbcol)  # Or try: terrain.colors(), viridis::viridis(nbcol), etc.

# Compute breaks and assign colors to z-values
z_facet <- z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]
z_facet <- z_facet / 4  # Average z value of each facet
facet_col <- color_palette[cut(z_facet, nbcol)]

# Plot with colors
png("Gamma2_surface.png", width = 800, height = 800)
persp(
  x = grid, y = grid, z = z,
  theta = 50, phi = 30, expand = 0.5,
  col = facet_col, ticktype = "detailed",
  xlab = "u (Lagged domain)", ylab = "s (Current domain)", zlab = "Covariance"
)
dev.off()
getwd()

#___________________________fpca_________________________

library(ftsa)
library(vars)

stock_grid <- 1:64

# Create functional time series object
train_weeks <- 1:146
test_weeks <- 147:156

fts_train <- fts(x = stock_grid, y = eval_matrix[,train_weeks])

fts_test <- fts(x = stock_grid, y = eval_matrix[,test_weeks])

model.ftsm <- ftsm(fts_train, order=3)
model.ftsm$varprop

png("residuals_ftsm_plot.png", width = 1000, height = 800)

plot(model.ftsm$residuals, 
     main = "Residuals",
     ylab = "Residuals",
     xlab = "Stock index",cex.main = 3,     # Increases main title size
     cex.lab = 2,    # Increases axis label size
     cex.axis = 1.5)

dev.off()
fts_test$y
forecast.ftsm <- predict(model.ftsm, h = 10)
names(forecast.ftsm)
png("ftsm_predited_plot.png", width = 1000, height = 800)
plot(forecast.ftsm$mean,
     main = "Prediction on Test set",
     xlab = "Stock Index",
     ylab = "Forecasted Value",
     cex.main = 3,
     cex.lab = 3,
     cex.axis = 1.5)
dev.off()


train_resid <- model.ftsm$residuals

# Mean squared error on training set
mse_train <- mean(train_resid$y^2, na.rm = TRUE)
print(paste("Train MSE:", round(mse_train, 6)))

y_forecast <- forecast.ftsm$mean$y     # Forecasted: matrix [stock points x 10]
y_actual   <- fts_test$y               # Actual: matrix [stock points x 10]

mse_test <- mean((y_actual - y_forecast)^2, na.rm = TRUE)
print(paste("Test MSE:", round(mse_test, 6)))
