library(quantmod)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(zoo)
install.packages("httr")
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
write.csv(fashion_stocks, file_path, row.names = FALSE)

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
write.csv(food_stoks, file_path, row.names = FALSE)


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
write.csv(travel_stocks, file_path, row.names = FALSE)

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
write.csv(oil_stocks, file_path, row.names = FALSE)

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
write.csv(logistics_stocks, file_path, row.names = FALSE)


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


it_stocks <- as.data.frame(lapply(it_stocks, fill_na_with_moving_avg))
nrow(it_stocks) # 780
sum(is.na(it_stocks))

file_path <- "C:/Users/adepa/OneDrive/Desktop/Functional Data Analysis/Functional-Data-Analysis-Project/it_stocks.csv"
write.csv(it_stocks, file_path, row.names = FALSE)


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
write.csv(automobile_stocks, file_path, row.names = FALSE)

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
write.csv(healthcare_stocks, file_path, row.names = FALSE)
library(ggplot2)


#Your file_path
path <- getwd()
setwd(file.path(getwd(), "data_stocks"))

#Read CSV
it <- read.csv("it_stocks.csv")
automobile <- read.csv("automobile_stocks.csv")
fashion<- read.csv("fashion_stocks.csv")
healthcare<- read.csv("healthcare_stocks.csv")
food <- read.csv("food_stoks.csv")
oil<- read.csv("oil_stocks.csv")
travel <- read.csv("travel_stocks.csv")
logistics <- read.csv("logistics_stocks.csv")

#Exclude rows for stocks higher than 776
logistics <-logistics[1:776,]
it <- it[1:776,]
automobile <- automobile[1:776,]
fashion <- fashion[1:776,]
healthcare <- healthcare[1:776,]
food <- food[1:776,]
oil <- oil[1:776,]
travel <- oil[1:776,]
#Merge all stocks
st <- cbind.data.frame(logistics,it,automobile,fashion,healthcare,food,oil,travel)
#Save the final file
write.csv(df, file = "final_data.csv", row.names = FALSE)
### Start ur code from here###
#Read the file
path <- getwd()
setwd(file.path(getwd(), "data_stocks"))
st <- read.csv("final_data.csv")
which(colnames(st) == "X005930.KS.Close")
#Exclude the stock 
st <- st[,-14]

#Visualize stocks for IT
opar <- par(mfrow=c(2,2))
plot(st$day, df$SPOT.Close, type="l", col="blue", lwd=2, xlab="", ylab="SPOT Close", 
     main="SPOT Close Prices")
plot(st$day, df$NFLX.Close, type="l", col="red", lwd=2, xlab="", ylab="NFLX Close", 
     main="NFLX Close Prices")
plot(st$day, df$NVDA.Close, type="l", col="green", lwd=2, xlab="", ylab="NVDA Close", 
     main="NVDA Close Prices")
plot(st$day, df$META.Close, type="l", col="purple", lwd=2, xlab="", ylab="META Close", 
     main="META Close Prices")
dev.off()


# DEPTH

library(DepthProc)
library(MultiRNG)


depth(mean(st),st)
depth(median(st),st)
median(st)
sort(depth(st,st),decreasing=TRUE) 

# Find maximal depth

depth_val <- numeric(776)
for(i in 1:776) {
  depth_val[i] <- depth(st[i], st)
}
#maximal depth
max(depth_val)

st[which.max(depth_val)]
median(st)

# Euclidean depth

dE <- depthEuclid(st, st)
mdE <- which.max(dE)
dE[mdE]
st[mdE,] # deepest point


apply(st,2,median)

plot(st)
points(st[mdE,1], st[mdE,2], pch=23, col="blue", bg="blue", lwd=2)
points(median(st[,1]), median(st[,2]), pch=24, col="red", bg="red", lwd=2)
# red one is median point while the blue one is the deepest point, affected by outliers


# Local depth
dL <- depthLocal(st, st, depth_params1 = list(method = "LP"))
dL
mdL <- which.max(dL)
dL[mdL]
st[mdL,]

depthContour(st, depth_params = list(method = "Local", depth_params1 = list(method = "LP"))) # 3D plot

# MBD, Frainman-Muniz
dMBD <- fncDepth(st, method = "MBD")
dFM <- fncDepth(st, method = "FM")
mdMBD <- which.max(dMBD)
mdFM <- which.max(dFM)

dMBD[mdMBD]
st[mdMBD,]

dFM[mdFM]
st[mdFM,]

plot(st, xlim=c(3,5), ylim=c(3,7))
points(st[mdMBD,1], st[mdMBD,2], pch=23, col="blue", bg="blue", lwd=2)
points(st[mdFM,1], st[mdFM,2], pch=23, col="green", bg="green", lwd=2)
points(median(st[,1]), median(st[,2]), pch=24, col="red", bg="red", lwd=2)

fncDepthMedian(st, method = "MBD")
fncDepthMedian(st, method = "FM")



#B-splines with penalty
library(fda)

library("fda.usc")

##B-splines
st <- as.matrix(st)
day <- c(1:776)

nrow(st)
#Create a grid for lambda and number of basis
l <- c(0 ,2^seq(-9, 9, len = 40))
nb <- seq(7, 40, by = 2)
time_points <- 1:776
#Create functional ojects with argumen values
fdata_obj <- fdata(t(st), argvals = time_points)
# Smooth with B-splines
out0 <- optim.basis(fdata_obj, lambda = l, numbasis = nb, type.basis = "bspline")
sum((fdata_obj$data - out0$fdata.est)^2)
basis <- create.bspline.basis(c(1,776),nbasis= out0$numbasis.opt, norder = 4)

#Calculate SSE
SSE <-sum((fdata_obj - out0$fdata.est )^2)


gcv = rep(0,40)
df = rep(0,40)
sse = rep(0,40)
#Iterate with different lambda for graph
lambda_seq = c(0 ,2^seq(-9, 9, len = 40))
for(i in 1:40){
  lambda=lambda_seq[i]
  tD2fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=lambda)
  
  smooth = smooth.basis(day, st, tD2fdPar)
  
  gcv[i] = sum(smooth$gcv)
  df[i] = smooth$df
  sse[i] = smooth$SSE
}

#Plot df, SSE and GCV
par(mfrow = c(3,1))
plot(0:39,df[1:40],type='l',xlab='log lambda',ylab='df',cex.lab=1.5)
plot(0:39,sse[1:40],type='l',xlab='log lambda',ylab='sse',cex.lab=1.5)
plot(0:39,gcv[1:40],type='l',xlab='log lambda',ylab='gcv',cex.lab=1.5)
dev.off()
#Find optimal lambda
optimal_lambda_index = which.min(gcv)
optimal_lambda = lambda_seq[optimal_lambda_index]
optimal_df = df[optimal_lambda_index]
optimal_sse = sse[optimal_lambda_index]
basis <- create.bspline.basis(c(1,776),nbasis= 39, norder = 4)
smooth = smooth.basis(day, st,basis )

smooth$fd

tD3fdPar = fdPar(basis,Lfdobj=int2Lfd(2),lambda=out0$lambda.opt)
smooth <- smooth.basis(day,st,tD3fdPar)
smooth$SSE
plot(smooth)

<<<<<<< HEAD

plot(out0$fdataobj)
names(out0$fdataobj)
dim(t(out0$fdataobj$data))
dim(t(st))
SSE <-sum((st - t(out0$fdataobj$data))^2)
st[1,]



<<<<<<< HEAD
=======


>>>>>>> parent of 7cf9c51 (Kernel updated)
=======
>>>>>>> 70544f7541eccaec1306643b96303d22992ef2d8
#Kernel smoothing
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



## Selected smoothing ##
out4 <- optim.np(fdata_obj, type.S = S.NW, h = 3:35, Ker = Ker.tri, correl = FALSE) #Triweight Kernel

#PCA
out4
library(fda)
fd_obj <- fdata2fd(out4$fdata.est)
nharm = 4
pcalist = pca.fd(fd_obj, centerfns = TRUE)
names(out4)

str(fd_obj)
class(fd_obj)

# EDA and outliers detection
smooth.fd = smooth$fd

plot(smooth.fd)

b_spline_mean = mean.fd(smooth.fd)
b_spline_sd = std.fd(smooth.fd)

lines(b_spline_mean, lwd=4, lty=2, col=2)
lines(b_spline_sd, lwd=4, lty=2, col=4)

lines(b_spline_mean-b_spline_sd, lwd=4, lty=2, col=6)
lines(b_spline_mean+b_spline_sd, lwd=4, lty=2, col=6)

lines(b_spline_mean-2*b_spline_sd, lwd=4, lty=2, col=8)
lines(b_spline_mean+2*b_spline_sd, lwd=4, lty=2, col=8)

