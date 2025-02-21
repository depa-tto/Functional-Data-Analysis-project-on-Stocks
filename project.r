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


