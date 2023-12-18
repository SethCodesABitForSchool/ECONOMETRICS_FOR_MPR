#Tutorial: Decomposing Time Series Data - Beveridge-Nelson Decomposition#

# 1. Install packages
install.packages("remotes")
remotes::install_github("KevinKotze/tsm")
install.packages("mFilter", repos = "https://cran.rstudio.com/", 
                 dependencies = TRUE)
install.packages("TSstudio")
library(tsm)
library(mFilter)
library(TSstudio)


# 2. Load the data
RGDP <- read_excel("DATA/RGDP.xls")
View(RGDP)
rgdp<-ts(RGDP$RGDP,start=c(1947,1,1),frequency=4) #set RGDP data as time series
View(rgdp)
Lrgdp = log(rgdp) #create a new variable as log of RGDP
ts_plot(Lrgdp, title="Time series Graph", Xtitle="Year", Ytitle="Variables", color="blue", width=3) 


# 3. Detrend data with the Beveridge-Nelson decomposition
bn.decomp <- bnd(Lrgdp, nlag = 10)  # apply the BN decomposition that creates dataframe

bn.trend <- ts(bn.decomp[, 1], start = c(1947, 1), frequency = 4)  # first column contains trend
bn.cycle <- ts(bn.decomp[, 2], start = c(1947, 1), frequency = 4)  # second column contains cycle

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(Lrgdp, ylab = "")
lines(bn.trend, col = "red")
legend("topleft", legend = c("data", "BNtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(bn.cycle, ylab = "")
legend("topleft", legend = c("BNcycle"), lty = 1, col = c("black"), 
       bty = "n")
