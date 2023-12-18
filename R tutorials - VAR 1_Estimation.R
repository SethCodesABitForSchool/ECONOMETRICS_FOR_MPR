# R tutorials - Introduction to Basic VAR Analysis: Concepts and Estimation #

# 1. Install packages
install.packages("devtools")
devtools::install_github("KevinKotze/tsm")
install.packages("vars")
install.packages("mFilter")
library(tsm)
library(vars)
library(mFilter)


# 2. Data preparation
quarterly <- read_excel("DATA/quarterly.xls")  
View (quarterly) 
gdp <- ts(quarterly$RGDP, start = c(1960, 1), freq = 4) # extract the GDP data into a time series format
une <- ts(quarterly$Unemp, start = c(1960, 1), freq = 4) # extract the unemployment rate data into a time series format
DLgdp = diff(log(gdp)) # the difference of the logarithm of GDP to get GDP growth rates


# 3. Data visualization
plot(cbind(DLgdp, une))
gdp.acf <- ac(DLgdp, main = "output") # create ACF plot for GDP growth rates
une.acf <- ac(une, main = "unemployent") # create ACF plot for unemployment rate


# 4. Unit root test - Dickey-Fuller test
adf.une <- ur.df(une, type = "trend", selectlags = "AIC")
summary(adf.une) # able to reject of unit root


# 5. VAR Model Selection
dat.bv <- cbind(DLgdp, une) # combine GDP growth rates and unemployment rate data
colnames(dat.bv) <- c("gdp", "une")

dat.bv <- dat.bv[complete.cases(dat.bv), ] # Subset the data to start from 1960Q2 and remove any missing values

info.bv <- VARselect(dat.bv, lag.max = 12, type = "const") # perform model selection using the VARselect function
info.bv$selection


# 6. Estimating and Interpreting the VAR model
bv.est <- VAR(dat.bv, p = 2, type = "const", season = NULL, exog = NULL) # estimating the VAR model 
summary(bv.est)

bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial
plot(bv.serial, names = "gdp")
plot(bv.serial, names = "une")
