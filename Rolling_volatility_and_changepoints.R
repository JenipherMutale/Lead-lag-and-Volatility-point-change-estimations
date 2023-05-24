#install the required packages
install.packages("yuima")
install.packages("quantmod")
install.packages("changepoint")
#load the libraries
library(quantmod)
library(changepoint)
library("xts")

# Get historical data for each cryptocurrencies
getSymbols("BTC-USD", from="2022-01-01", to="2022-12-31", src="yahoo", periodicity="daily")
getSymbols("ETH-USD", from="2022-01-01", to="2022-12-31", src="yahoo", periodicity="daily")
getSymbols("SOL-USD", from="2022-01-01", to="2022-12-31", src="yahoo", periodicity="daily")
getSymbols("MATIC-USD", from="2022-01-01", to="2022-12-31", src="yahoo", periodicity="daily")

# Extract the closing price data  
Close_BTC<- as.numeric(Cl(`BTC-USD`)) 
Close_ETH<-as.numeric( Cl(`ETH-USD`))
Close_SOL<- as.numeric(Cl(`SOL-USD`))
Close_MATIC<- as.numeric(Cl(`MATIC-USD`))

# Compute the logarithmic returns for each crypto
#the function "diff" takes the difference of two logarithm terms

returns_BTC <- diff(log(Close_BTC))
returns_ETH <- diff(log(Close_ETH))
returns_SOL <- diff(log(Close_SOL))
returns_MATIC <- diff(log(Close_MATIC))

############
#computing the lead-lag matrix and the correlation coeficients
#merge the closing prices into one data frame
Crypto_data<- merge(Close, Close1, Close2, Close3)
colnames(Crypto_data) <- c("BITCOIN","ETHEREUM", "SOLANA","POLYGON")

Crypto <- setYuima(data=setData(Crypto_data, delta=1/365))
#computing the lead-lag matrix
round(llag(Crypto),4) 
# Compute correlation between the two assets
correlation <- cor(Crypto_data)



#ploting the closing prices of crypto data
par(mfrow=c(2,2))
plot(Bitcoin,mainlab="Bitcoin Closing Prices",
     col = "blue")
plot(Ethereum, mainlab="Ethereun Closing Prices",
     col = "green")
plot(Solana, mainlab="Solana Closing Prices",
     col = "purple")
plot(Polygon, mainlab="Polygon Closing Prices",
     col = "black")
# Create separate boxplots for each cryptocurrency uising their log retuns
par(mfrow = c(2, 2))
boxplot(returns_BTC,
        main = "Log Returns for Bitcoin",
        ylab = "Log Returns",
        col = "blue")
boxplot(returns_ETH,
        main = "Log Returns for Ethereum",
        ylab = "Log Returns",
        col = "green")
boxplot(returns_SOL,
        main = "Log Returns for Solana",
        ylab = "Log Returns",
        col = "purple")
boxplot(returns_MATIC,
        main = "Log Returns for Polygon",
        ylab = "Log Returns",
        col = "black")
#ploting the log returns for the cryptocurrencies
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

# Compute rolling window volatility for each crypto currency using a window size of 10 days
volatility_BTC <-na.omit(volatility(Close_BTC, n = 10)) 
volatility_ETH <- na.omit(volatility(Close_ETH, n = 10))
volatility_SOL <- na.omit(volatility(Close_SOL, n = 10))
volatility_MATIC <- na.omit(volatility(Close_MATIC, n = 10))

# Estimate the change point in volatility using the 'cpt.var' function
BTC_cpt <- cpt.var(volatility_BTC, method="PELT")
ETH_cpt = cpt.var(volatility_ETH, method="PELT")
SOL_cpt = cpt.var(volatility_SOL, method="PELT")
MATIC_cpt = cpt.var(volatility_MATIC, method="PELT")

# Plot the change point results
par(mfrow=c(2,2))
plot(BTC_cpt, col="blue", xlab="Time", ylab="Volatility", main="Volatility Change Point Estimation for BTC Stock Price")
plot(ETH_cpt, col="green", xlab="Time",ylab="Volatility")
plot(SOL_cpt, col="purple", xlab="Time", ylab="Volatility")
plot(MATIC_cpt, col="black", xlab="Time", ylab="Volatility")

#plotting the change points with the number of times they experienced a change
# create an empty plot
plot(1, type="n", xlim=c(1, max(sapply(list(BTC_cpt, ETH_cpt, SOL_cpt, MATIC_cpt), function(x) length(x@cpts)))), ylim=c(0, max(sapply(list(BTC_cpt, ETH_cpt, SOL_cpt, MATIC_cpt), function(x) max(x@cpts)))) , xlab="Change Points", ylab="Days", main="Change Point Estimation for Cryptocurrencies")
# add points for each cryptocurrency change point 
points(1:length(BTC_cpt@cpts), BTC_cpt@cpts, col="blue", pch=16)
points(1:length(ETH_cpt@cpts), ETH_cpt@cpts, col="green", pch=16)
points(1:length(SOL_cpt@cpts), SOL_cpt@cpts, col="purple", pch=16)
points(1:length(MATIC_cpt@cpts), MATIC_cpt@cpts, col="black", pch=16)




