# initialize
# clear workspace
rm(list = ls())
graphics.off()
# load libraries and data
library(fredr)
#Get API key by registering on the FREED website and optain an API. then set the API key in R as follows
FRED_API_KEY <- fredr_set_key("YOUR_KEY_HERE")  #this sets the API. 
fredr_has_key()      #returns TRUE if a key can be found.
fredr_get_key()     #returns the API key set
#get CPI data
US_CPI <- fredr(
  series_id = "CPILFESL",
  observation_start = as.Date("1957-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "m", # for monthly
  units = "lin") # levels of data. no transformation. if you need change replace with "chg".

View(US_CPI)
#drop real time start and real time end
US_CPI <- subset(US_CPI, select = -c(series_id, realtime_start, realtime_end))
View(US_CPI)
#rename "value" as CPI
colnames(US_CPI)[2] <- "CPI" 

# get US Unemployment rate
US_UNRATE <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1957-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "m", # for monthly
  units = "lin") # levels of data. no transformation. if you need change replace with "chg".

View(US_UNRATE)
#drop real time start and real time end
US_UNRATE <- subset(US_UNRATE, select = -c(series_id, realtime_start, realtime_end))
View(US_UNRATE)
#rename "value" as CPI
colnames(US_UNRATE)[2] <- "UNRATE" 

# Construct inflation rate

library(TSstudio)
library(tidyverse)
library(zoo)

cpi<-ts(US_CPI$CPI,start=c(1957,1,1),frequency=12) #create TS variable from dataset
ts_plot(cpi)
library(quantmod)
library(dplyr)
cpi_infl<-Delt(cpi,k=12,type="arithmetic") #calculate Y-o-Y inflation at monthly fre
cpi_infl<-window(cpi_infl,start=c(1958,1))
cpi_infl<-cpi_infl*100  # in percent
ts_plot(cpi_infl)

# plot unemployment rate
unrate<-ts(US_UNRATE$UNRATE,start=c(1957,1,1),frequency=12) #create TS variable from dataset
unrate<-window(unrate,start=c(1958,1))
ts_plot(unrate)

# plot unemployment and inflation in one chart
plot(cbind(cpi_infl, unrate))

# diagnostics
library(tsm)

infl.acf <- ac(as.numeric(cpi_infl), max.lag = 48,main = "inflation")
unrate.acf <- ac(as.numeric(unrate), max.lag = 48,main = "unemployment")

# VAR Model selection and estimation
library(vars)
library(mFilter)
# create bivariate object and use information criteria
dat.bv <- cbind(cpi_infl, unrate)
colnames(dat.bv) <- c("inflation", "unemployment")

info.bv <- VARselect(dat.bv, lag.max = 12, type = "const")
info.bv$selection

# let's pick the more parsimonious model (p=3-lag) according to HQC & SBC
# Estimate VAR
bv.est <- VAR(dat.bv, p = 3, type = "const", season = NULL, exog = NULL)
summary(bv.est)
# Note that characteristic roots are within the unit circle -> system is stable

# residual diagnostics
bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial
# p-value is <.05 so evidence for serial correlation

plot(bv.serial, names = "inflation")
plot(bv.serial, names = "unemployment")

# To keep it simple, we'll proceed regardless to forecast (i.e. more lags should be added)
# 8-step ahead forecast w/ 95 confidence interval

predictions <- predict(bv.est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "unemployment")
plot(predictions, names = "inflation")

# Display a fanchart for the forecasts
fanchart(predictions, names = "unemployment")
fanchart(predictions, names = "inflation")
