install.packages("FRAPO")
install.packages("copula")
library(copula)

library(quantmod)

library(yuima)

library(FRAPO)




# Get historical data for each asset

getSymbols("BTC-USD", from = "2022-01-01", to = "2022-12-31", src = "yahoo", periodicity = "daily")

getSymbols("ETH-USD", from = "2022-01-01", to = "2022-12-31", src = "yahoo", periodicity = "daily")

getSymbols("SOL-USD", from = "2022-01-01", to = "2022-12-31", src = "yahoo", periodicity = "daily")

getSymbols("MATIC-USD", from = "2022-01-01", to = "2022-12-31", src = "yahoo", periodicity = "daily")





# Extract the closing price data

Close_BTC<- Cl(`BTC-USD`)

Close_ETH<-Cl(`ETH-USD`)

Close_SOL<- Cl(`SOL-USD`)

Close_MATIC<-Cl(`MATIC-USD`)



# Compute the logarithmic returns for each crypto

returns_BTC <- diff(log(Close_BTC))

returns_ETH <- diff(log(Close_ETH))

returns_SOL <- diff(log(Close_SOL))

returns_MATIC <- diff(log(Close_MATIC))


# Create a data frame to store the closing prices
closing_prices1 <- na.omit(data.frame(returns_BTC, returns_ETH, returns_SOL, returns_MATIC))

# Rename the columns
colnames(closing_prices1) <- c("Bitcoin", "Ethereum", "Solana", "Polygon")
# Compute the correlation matrix
correlation_matrix1 <- cor(closing_prices1)

# View the correlation matrix
print(correlation_matrix1)


par(mfrow=c(2,2))
plot(returns_BTC,main = "Returns_BTC",
     ylab = "Log Returns",
     col = "blue")

plot(returns_ETH,main = "Returns_ETH",
     ylab = "Log Returns",
     col = "green")
plot(returns_SOL,main = "Returns_SOL",
     ylab = "Log Returns",
     col = "purple")

plot(returns_MATIC,main = "Returns_MATIC",
     ylab = "Log Returns",
     col = "black")


