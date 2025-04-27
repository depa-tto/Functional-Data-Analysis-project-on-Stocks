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

# exclude rows for stocks higher than 756
logistics <-logistics[1:756,]
it <- it[1:756,]
automobile <- automobile[1:756,]
fashion <- fashion[1:756,]
healthcare <- healthcare[1:756,]
food <- food[1:756,]
oil <- oil[1:756,]
travel <- oil[1:756,]
# merge all stocks
st <- cbind.data.frame(logistics,it,automobile,fashion,healthcare,food,oil,travel)
# save the final file
write.csv(st, file = "final_data.csv", row.names = FALSE)

########################################################################## start ur code from here ##########################################################################
library(quantmod)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(zoo)
library(httr)
library(DepthProc)
library(MultiRNG)
library(fda)
library(fda.usc)
library(fdaoutlier)
library(fda)

# Read the file
path <- getwd()
setwd(file.path(getwd(), "data_stocks"))
st <- read.csv("final_data.csv")
head(st)
sum(is.na(st))

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

st <- as.matrix(st)
sum(is.na(st))

# Euclidean depth

dE <- depthEuclid(st, st) # dE is the vector of depth values for each observation
dE
mdE <- which.max(dE)
mdE # index of the maximum depth on dE
dE[mdE] # euclidean depth of the deepest point in the dataset

# a very small depth value (like this one) suggests that the dataset 
# is highly spread out or has strong outliers, meaning even the "deepest" point is not very central.

st[mdE,] # retrieves the most central (deepest) row from the dataset based on Euclidean Depth
apply(st,2,median) # median of each column in the dataset

# if st[mdE, ] and apply(st, 2, median) are close, the deepest point is 
# near the median, suggesting symmetric distribution
# If they differ significantly, the data might be skewed or have outliers


plot(st)
points(st[mdE,1], st[mdE,2], pch=23, col="blue", bg="blue", lwd=5)
points(median(st[,1]), median(st[,2]), pch=24, col="red", bg="red", lwd=5)
legend("topright", legend=c("Deepest Point", "Median Point"),
       pch=c(23, 24), col=c("blue", "red"), pt.bg=c("blue", "red"), cex=1,horiz=TRUE)
# red one is median point while the blue one is the deepest point, affected by outliers
# if the blue (deepest) and red (median) points are close, the dataset is relatively symmetric and balanced.
# if they are far apart, the dataset might have outliers or skewed distribution (the Euclidean Depth is influenced by the overall spread of the data)




# Local depth
dL <- depthLocal(st, st, depth_params1 = list(method = "LP"))
mdL <- which.max(dL)
dL[mdL]
st[mdL,]

depthContour(st[,1:2], depth_params = list(method = "Local", depth_params1 = list(method = "LP")))


# MBD, Frainman-Muniz
dMBD <- fncDepth(st, method = "MBD")
dFM <- fncDepth(st, method = "FM")
mdMBD <- which.max(dMBD)
mdFM <- which.max(dFM)

dMBD[mdMBD] # deepest points
st[mdMBD,] 

dFM[mdFM]  # deepest points
st[mdFM,]

# dMBD[mdMBD] and dFM[mdFM] display the depth values of the deepest points according 
# to the MBD and FM methods.
# st[mdMBD,] and st[mdFM,] display the actual data points corresponding to these deepest points

dev.off()
plot(st)
points(st[mdMBD,1], st[mdMBD,2], pch=23, col="blue", bg="blue", lwd=5)
points(st[mdFM,1], st[mdFM,2], pch=23, col="green", bg="green", lwd=5)
points(median(st[,1]), median(st[,2]), pch=24, col="red", bg="red", lwd=5)

fncDepthMedian(st, method = "MBD")
fncDepthMedian(st, method = "FM")
# These lines compute the median depth for the data using MBD and FM methods. 
# This gives a central point based on each depth method, similar to calculating 
# the median of the dataset but based on depth rather than simple averaging.

### B-splines
st <- as.matrix(st)
day <- c(1:756)
nrow(st)

# Create a grid for lambda and number of basis
l <- c(0 ,2^seq(-9, 9, len = 40))
nb <- seq(7, 40, by = 2)
time_points <- 1:756

# Create functional ojects with argumen values
fdata_obj <- fdata(t(st), argvals = time_points)


# Smooth with B-splines
out0 <- optim.basis(fdata_obj, lambda = l, numbasis = nb, type.basis = "bspline")
sum((fdata_obj$data - out0$fdata.est)^2)
basis <- create.bspline.basis(c(1,756),nbasis= out0$numbasis.opt, norder = 4) # cubic splines

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
plot(0:39,df[1:40],type='l',xlab='log lambda',ylab='df',cex.lab=1.5) # shows how model complexity changes with 𝜆
plot(0:39,sse[1:40],type='l',xlab='log lambda',ylab='sse',cex.lab=1.5) # measures fit quality
plot(0:39,gcv[1:40],type='l',xlab='log lambda',ylab='gcv',cex.lab=1.5) # helps choose the best 𝜆(minimum GCV)
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
optimal_lambda

optimal_df = df[optimal_lambda_index]
optimal_sse = sse[optimal_lambda_index]
basis <- create.bspline.basis(c(1,776),nbasis= out0$numbasis.opt, norder = 4)


tD3fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=out0$lambda.opt)
smooth <- smooth.basis(day,st,tD3fdPar)
smooth$SSE
plot(smooth$fd)

plot(out0$fdataobj)
names(out0$fdataobj)
dim(t(out0$fdataobj$data))
dim(t(st))
SSE <-sum((st - t(out0$fdataobj$data))^2)
st[1,]


# EDA and outliers detection for b-spline
smooth.fd = smooth$fd

plot(smooth.fd)

# Calculate mean and standard deviation
b_spline_mean = mean.fd(smooth.fd)
b_spline_sd = std.fd(smooth.fd)

# Plot the mean and standard deviation lines
lines(b_spline_mean, lwd=4, lty=2, col=2)
lines(b_spline_sd, lwd=4, lty=2, col=4)

# Plot the mean ± SD and mean ± 2SD lines
lines(b_spline_mean-b_spline_sd, lwd=4, lty=2, col=6)
lines(b_spline_mean+b_spline_sd, lwd=4, lty=2, col=6)

lines(b_spline_mean-2*b_spline_sd, lwd=4, lty=2, col=8)
lines(b_spline_mean+2*b_spline_sd, lwd=4, lty=2, col=8)

# Add the legend
legend("topright", 
       legend=c("Mean", "SD", "Mean ± SD", "Mean ± 2SD"),
       col=c(2, 4, 6, 8), 
       lty=4, 
       lwd=6, 
       box.lwd=3,
      cex=2)

# the Bivariate Covariance Function v(s; t)
# this function captures how the variability of the smoothed functional data changes over time

logprecvar.bifd = var.fd(smooth.fd)

# Contour plot every 5 days

day5time = seq(1,756,5)
logprec.varmat = eval.bifd(day5time, day5time,logprecvar.bifd)

persp(day5time, day5time, logprecvar_mat,
      theta=-45, phi=25, r=3, expand = 0.5,
      ticktype='detailed',
      xlab="Day",
      ylab="Day",
      zlab="variance(log10 precip)")


contour(day5time, day5time, logprec.varmat,
        xlab="Day",
        ylab="Day", lwd=2,
        labcex=1)

### Outlier Detection using Functional Boxplot

boxplot(smooth.fd)


### Kernel smoothing
out1 <- optim.np(fdata_obj , type.S = S.NW, par.CV = list(criteria = "GCV"))#Local regression
out2 <- optim.np(fdata_obj, type.S = S.LLR, par.CV = list(criteria = "GCV"))#Local kernel
out3 <- optim.np(fdata_obj, type.S = S.KNN, h = 3:35, Ker = Ker.norm) # Normal Kernel
out4 <- optim.np(fdata_obj, type.S = S.NW, h = 3:35, Ker = Ker.tri, correl = FALSE) #Triweight Kernel
out5 <- optim.np(fdata_obj, type.S = S.NW, h = 3:35, Ker = Ker.epa, correl = FALSE) #Epanechnikov Kerne
out6 <- optim.np(fdata_obj, type.S = S.NW, h = 3:35, Ker = Ker.unif, correl = FALSE) #Uniform Kernel


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
methods <- c("out0", "out1", "out2", "out3", "out4", "out5", "out6")

# Plot GCV values
barplot(gcv_values, names.arg = methods, col = "lightblue", main = "GCV Comparison",
        ylab = "GCV Value", xlab = "Methods", las = 2)

# Compute Sum of Squared Errors (SSE) for each method
sse_values <- c(SSE_out1, SSE_out2, SSE_out3, SSE_out4, SSE_out5, SSE_out6)

# Plot SSE values
barplot(sse_values, names.arg = methods[-1], col = "lightcoral", main = "SSE Comparison",
        ylab = "Sum of Squared Errors", xlab = "Methods", las = 2)

par(mfrow = c(1, 1))  # Reset layout to default


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
plot(out0$fdata.est[4,],col ="blue", lwd = 3)
points(st[,4], col = "red")

plot(out3$fdata.est[4,],col ="blue", lwd = 3)
points(st[,4], col = "red")



### Selected smoothing ###

basis <- create.bspline.basis(c(1,776),nbasis= out0$numbasis.opt, norder = 4)
tD3fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=out0$lambda.opt)
smooth <- smooth.basis(day,st,tD3fdPar)



#PCA

library(fda)


nharm = 4
pcalist = pca.fd(smooth.fd, nharm, centerfns = TRUE) # first 4 principal components (nharm = 4), centered around the mean curve
plot(pcalist)
plot(pcalist$harmonics)

plotscores(pcalist, loc = 5)

fd.pca1.list <- list() 
fd.pca2.list <- list() 
fd.pca3.list <- list() 
fd.pca4.list <- list() 

# The following blocks of code reconstruct functional data using the PCA 
# scores, step by step, starting with the mean curve and adding the 
# components progressively:

for(i in 1:5) {
  fd.pca1.list[[i]] <- mean.fd(smooth.fd) + 
    pcalist$scores[i,1]*pcalist$harmonics[1]
  
  fd.pca2.list[[i]] <- mean.fd(smooth.fd) + 
    pcalist$scores[i,1]*pcalist$harmonics[1] + 
    pcalist$scores[i,2]*pcalist$harmonics[2]
  
  fd.pca3.list[[i]]<- mean.fd(smooth.fd) +
    pcalist$scores[i,1]*pcalist$harmonics[1] + 
    pcalist$scores[i,2]*pcalist$harmonics[2] +
    pcalist$scores[i,3]*pcalist$harmonics[3] 
  
  fd.pca4.list[[i]]<- mean.fd(smooth.fd) +
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
  lines(smooth.fd[i], col = 2)
  
  plot(fd.pca2.list[[i]], ylim=c(-1, 1), ylab = "2 PC")
  lines(smooth.fd[i], col = 2)
  
  plot(fd.pca3.list[[i]], ylim=c(-1, 1), ylab = "3 PC")
  lines(smooth.fd[i], col = 2)
  
  plot(fd.pca4.list[[i]], ylim=c(-1, 1), ylab = "4 PC")
  lines(smooth.fd[i], col = 2)
}
par(opar)

varmx <- varmx.pca.fd(pcalist)
plot(varmx)
par(opar)

plot(varmx$harmonics)

plotscores(varmx, loc = 5)


# PCA restore the original curves
fd.vrm1.list <- list() 
fd.vrm2.list <- list() 
fd.vrm3.list <- list() 
fd.vrm4.list <- list() 

for(i in 1:5) {
  fd.vrm1.list[[i]] <- mean.fd(smooth.fd) + 
    varmx$scores[i,1]*varmx$harmonics[1]
  
  fd.vrm2.list[[i]] <- mean.fd(smooth.fd) +
    varmx$scores[i,1]*varmx$harmonics[1] + 
    varmx$scores[i,2]*varmx$harmonics[2]
  
  fd.vrm3.list[[i]]<- mean.fd(smooth.fd) +
    varmx$scores[i,1]*varmx$harmonics[1] + 
    varmx$scores[i,2]*varmx$harmonics[2] +
    varmx$scores[i,3]*varmx$harmonics[3] 
  
  fd.vrm4.list[[i]]<- mean.fd(smooth.fd) +
    varmx$scores[i,1]*varmx$harmonics[1] + 
    varmx$scores[i,2]*varmx$harmonics[2] +
    varmx$scores[i,3]*varmx$harmonics[3] +
    varmx$scores[i,4]*varmx$harmonics[4]
}

opar <- par(mfrow=c(2,2), ask = TRUE)
for(i in 1:5) {
  plot(fd.vrm1.list[[i]], ylim=c(-1, 1), ylab = "1 PC")
  lines(smooth.fd[i], col = 2)
  
  plot(fd.vrm2.list[[i]], ylim=c(-1, 1), ylab = "2 PC")
  lines(smooth.fd[i], col = 2)
  
  plot(fd.vrm3.list[[i]], ylim=c(-1, 1), ylab = "3 PC")
  lines(smooth.fd[i], col = 2)
  
  plot(fd.vrm4.list[[i]], ylim=c(-1, 1), ylab = "4 PC")
  lines(smooth.fd[i], col = 2)
}
par(opar)


### Kernel smoothing

out1 <- optim.np(fdata_obj , type.S = S.NW, par.CV = list(criteria = "GCV")) # Local regression
out2 <- optim.np(fdata_obj, type.S = S.LLR, par.CV = list(criteria = "GCV")) # Local kernel
out3 <- optim.np(fdata_obj, type.S = S.KNN, h = 3:35, Ker = Ker.norm) # Normal Kernel
out4 <- optim.np(fdata_obj, type.S = S.NW, h = 3:35, Ker = Ker.tri, correl = FALSE) # Triweight Kernel
out5 <- optim.np(fdata_obj, type.S = S.NW, h = 3:35, Ker = Ker.epa, correl = FALSE) # Epanechnikov Kerne
out6 <- optim.np(fdata_obj, type.S = S.NW, h = 3:35, Ker = Ker.unif, correl = FALSE) # Uniform Kernel


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
methods <- c("out0", "out1", "out2", "out3", "out4", "out5", "out6")

# Plot GCV values
barplot(gcv_values, names.arg = methods, col = "lightblue", main = "GCV Comparison",
        ylab = "GCV Value", xlab = "Methods", las = 2)

# Compute Sum of Squared Errors (SSE) for each method
sse_values <- c(SSE_out1, SSE_out2, SSE_out3, SSE_out4, SSE_out5, SSE_out6)

# Plot SSE values
barplot(sse_values, names.arg = methods[-1], col = "lightcoral", main = "SSE Comparison",
        ylab = "Sum of Squared Errors", xlab = "Methods", las = 2)

par(mfrow = c(1, 1)) # Reset layout to default


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

### Plotting the differet smoothing 
lines(st[,1], col = "red")
par(mfrow = c(1,2))
plot(out0$fdata.est[4,],col ="blue", lwd = 3)
points(st[,4], col = "red")

plot(out3$fdata.est[4,],col ="blue", lwd = 3)
points(st[,4], col = "red")


### Selected smoothing ###

basis <- create.bspline.basis(c(1,776),nbasis= out0$numbasis.opt, norder = 4)
tD3fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=out0$lambda.opt)
smooth <- smooth.basis(day,st,tD3fdPar)



#PCA
out4
library(fda)
fd_obj <- fdata2fd(out4$fdata.est)
nharm = 4
pcalist = pca.fd(fd_obj, centerfns = TRUE)
names(out4)

str(fd_obj)
class(fd_obj)

plot(out3$fdata.est, main = "Smoothed Functional Data") # smoothed functions

mean_kernel <- plot(mean(out3$fdata.est))

plot(out3$fdata.est, main = "Smoothed Functional Data")


lines(b_spline_mean-2*b_spline_sd, lwd=4, lty=2, col=8)
lines(b_spline_mean+2*b_spline_sd, lwd=4, lty=2, col=8)

