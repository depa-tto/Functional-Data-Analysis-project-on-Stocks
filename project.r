library(quantmod)  

# Download packages
install.packages("tidyquant")  
install.packages("quantmod")    
install.packages("ggplot2")
install.packages("BatchGetSymbols")
library("tidyquant")
#Car&Fashion industry
Fashion  <- c("KER.PA", "CPRI", "RMS.PA", "MC.PA", "CFR.SW", "ADS.DE", "NKE.DE", "PUM.DE") 

fashion_data <- tq_get(Fashion, from = "2020-01-01", to = "2022-12-31")

close_fashion <- fashion_data[,c("date", "symbol", "close")]

oil <- c("CHV.F", "REP.MC","TTE.PA")

oil_data <- tq_get(oil, from = "2020-01-01", to = "2022-12-31")
close_oil <- oil_data[, c("date", "symbol", "close")]

