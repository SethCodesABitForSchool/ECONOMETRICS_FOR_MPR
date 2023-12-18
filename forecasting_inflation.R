rm(list = ls())
graphics.off()
# Forecasting with ARMA models
library(tsm)
library(strucchange)
library(forecast)
#drop missing obs
head(cpi_infl_qtly)
cpi_infl_qtly<-window(cpi_infl_qtly,start=c(1958,1))
head(cpi_infl_qtly)
# acf/pacf plots
acf(cpi_infl_qtly,lag.max=48)
pacf(cpi_infl_qtly,lag.max=48)
# alternative
ac(as.numeric(cpi_infl_qtly), max.lag = 48)

# lag selection
arma.aic <- rep(0, 5)
arma.res <- rep(0, 5)
arma.res[1] <- arima(cpi_infl_qtly, order = c(2, 0, 0))$aic
arma.res[2] <- arima(cpi_infl_qtly, order = c(5, 0, 0))$aic
arma.res[3] <- arima(cpi_infl_qtly, order = c(6, 0, 0))$aic
arma.res[4] <- arima(cpi_infl_qtly, order = c(13, 0, 0))$aic
arma.res[5] <- arima(cpi_infl_qtly, order = c(17, 0, 0))$aic
head(arma.res)
# find model with smallest aic
which(arma.res == min(arma.res))
#alternatively
ar2<-arima(cpi_infl_qtly,order=c(2,0,0))
ar5<-arima(cpi_infl_qtly,order=c(5,0,0))
ar6<-arima(cpi_infl_qtly,order=c(6,0,0))
ar13<-arima(cpi_infl_qtly,order=c(13,0,0))
ar17<-arima(cpi_infl_qtly,order=c(17,0,0))
#aic
arma.aic[1]<-AIC(ar2)
arma.aic[2]<-AIC(ar5)
arma.aic[3]<-AIC(ar6)
arma.aic[4]<-AIC(ar13)
arma.aic[5]<-AIC(ar17)
# find model with smallest aic
which(arma.aic == min(arma.aic))
head(arma.aic)
#bic
arma.bic <- rep(0, 5)

arma.bic[1]<-BIC(ar2)
arma.bic[2]<-BIC(ar5)
arma.bic[3]<-BIC(ar6)
arma.bic[4]<-BIC(ar13)
arma.bic[5]<-BIC(ar17)
which(arma.bic == min(arma.bic))
head(arma.bic)

# pick model 5 & perform residual diagnostics
par(mfrow=c(1,1))
plot(ar17$residuals)
ac(ar17$residuals)

# use model 3 for forecast model evaluation purposes
plot(ar6$residuals)
ac(ar6$residuals)

# Recursive forecast generation and evaluation
# set up matrices for data and forecasts (row=period, column=step-ahead forecast, up to 8 quaters)
# out of sample = 12 years = 48 observations
H <- 8 #forecast horizon
P <- 48 #out-of-sample 
obs<-length(cpi_infl_qtly)
R <- obs -(P+H)+1
# actual data matrix
actual <-matrix(data=rep(0,P*H),ncol = H)

dates <- as.numeric(time(cpi_infl_qtly))
dates[R]  # last initial in-sample observation

# assign dates to matrix
for (i in 1:P) {
  first <- R + i
  last <- first + H - 1
  actual[i, 1:H] <- dates[first:last]
}
# fill actual data in each cell
actual <- matrix(data = rep(0, P * H), ncol = H)

for (i in 1:P) {
  first <- R + i
  last <- first + H - 1
  actual[i, 1:H] <- cpi_infl_qtly[first:last]
}
# create similar matrix to store forecasts from AR(17) model
fore.res <- matrix(data = rep(0, P * H), ncol = H)
# need to set subsample for each AR(17) model. EStimate parameters with arima
# and forecast 8-steps ahead with predict. Save into fore.res
for (i in 1:P) {
  last.obs <- R + i - 1
  cpi_infl_qtly.sub <- cpi_infl_qtly[1:last.obs]
  
  arma.est <- arima(cpi_infl_qtly.sub, order = c(17, 0, 0))
  arma.fore <- predict(arma.est, n.ahead = H)
  
  fore.res[i, 1:H] <- arma.fore$pred
}
# look at one of the 8-step ahead forecasts. For examine, view the 8th-row
rowf <- 8

par(mfrow = c(1, 1))
# plot forecast vs data
plot.ts(actual[rowf, ], col = "black", ylab = "Example", 
        xlab = "Steps", ylim = c(0, max(actual[rowf, ] * 1.5)))
lines(fore.res[rowf, ], col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("actual", "forecast"), lty = c(1,2), col = c("black", "red"), bty = "n")

#create matrix of Forecast Errors
error <- actual - fore.res
# calculate bias of each step ahead forecast (i,e. mean of values for each column in error matrix)
bias.step <- rep(0, H)

for (i in 1:H) {
  bias.step[i] <- mean(error[, i])
}

print(bias.step)

#calculate RMSE for each step ahead & over time
RMSE.step <- sqrt(colMeans(error^2))
RMSE.time <- sqrt(rowMeans(error^2))

# plot RMSE results over time
plot(ts(RMSE.time, start = c(2011, 1), frequency = 4), ylab = "RMSE time", 
     type = "o", col = "red")
# Notice the poor forecasts during the Covid-19 pandemic 

# plot size of RMSE for each step
plot(RMSE.step, ylab = "RMSE step", type = "o", col = "red")
# Notice that size of error grows rapidly after 3 step

### Forecast tests
# compare AR6 vs AR17
# create matrix for new set of forecasts
fore.alt <- matrix(data = rep(0, P * H), ncol = H)

for (i in 1:P) {
  last.obs <- R + i - 1
  cpi_infl_qtly.sub <- cpi_infl_qtly[1:last.obs]
  
  arma.alt <- arima(cpi_infl_qtly.sub, order = c(6, 0, 0))
  arma.falt <- predict(arma.alt, n.ahead = H)
  
  fore.alt[i, 1:H] <- arma.falt$pred
}

#calculate forecast errors
error.alt <- fore.alt - actual

RMalt.step <- sqrt(colSums(error.alt^2))  # averaged over time
RMalt.time <- sqrt(rowSums(error.alt^2))  # averaged over all 8 steps

# plot both RMSEs for comparison
plot.ts(RMSE.step, xlab = "Steps", ylab = "RMSE", ylim = c(0, max(c(RMSE.step, RMalt.step))))
lines(RMalt.step, col = "red", lty = 2)
legend("topright", legend = c("AR(17)", "AR(6)"), lty = c(1,2), col = c("black", "red"), bty = "n")

# RMSE time plot
plot(ts(RMSE.time, start = c(2011, 1), frequency = 4), ylab = "RMSE", 
     ylim = c(min(c(RMSE.time, RMalt.time)), max(c(RMSE.time, 
                                                   RMalt.time))))
lines(ts(RMalt.time, start = c(2011, 1), frequency = 4), 
      col = "red", lty = 2)
legend("topright", legend = c("AR(17)", "AR(6)"), lty = c(1,2), col = c("black", "red"), bty = "n")

# determine whether the difference is significant
# Since models are nested, instead of using Diebold-Mariano test we use Clarke-West test
# Note: You will need tsm package
# save in vectors for test stat & pvalue
cwstat.steps <- rep(0, H)
cwpval.steps <- rep(0, H)

for (i in 1:H) {
  cwstat.steps[i] <- cw(error[, i], error.alt[, i], fore.res[, 
                                                             i], fore.alt[, i])$test
  cwpval.steps[i] <- cw(error[, i], error.alt[, i], fore.res[, 
                                                             i], fore.alt[, i])$pvalue
}
#plot test results
plot(cwstat.steps, ylim = c(-5, 5), xlab = "Steps", ylab = "CW statistic")
lines(rep(0, H), col = "grey", lty = 1)
lines(rep(1.96, H), col = "red", lty = 2)
lines(rep(-1.96, H), col = "red", lty = 2)

# statistics can also be computed at each point in time
cwstat.time <- rep(0, P)
cwpval.time <- rep(0, P)

for (i in 1:P) {
  cwstat.time[i] <- cw(error[i, ], error.alt[i, ], fore.res[i, 
  ], fore.alt[i, ])$test
  cwpval.time[i] <- cw(error[i, ], error.alt[i, ], fore.res[i, 
  ], fore.alt[i, ])$pvalue
}

# plot results
plot(ts(cwstat.time, start = c(2011, 1), frequency = 4), 
     type = "p", ylim = c(-25, 25), ylab = "CW statistic")
lines(ts(rep(0, P), start = c(2011, 1), frequency = 4), 
      col = "grey", lty = 1)
lines(ts(rep(1.96, P), start = c(2011, 1), frequency = 4), 
      col = "red", lty = 2)
lines(ts(rep(-1.96, P), start = c(2011, 1), frequency = 4), 
      col = "red", lty = 2)