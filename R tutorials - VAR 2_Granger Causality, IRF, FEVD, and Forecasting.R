# R tutorials - Exploring VAR Analysis: Granger Causality, Impulse Responses, FEVD, and Forecasting #

# 1. Install packages
# 2. Data preparation
# 3. Data visualization
# 4. Unit root test - Dickey-Fuller test
# 5. VAR Model Selection
# 6. Estimating and Interpreting the VAR model


# 7. Granger causality analysis
bv.cause.gdp <- causality(bv.est, cause = "gdp") # performing Granger Causality anlaysis for 'gdp' causing 'une'
bv.cause.gdp

bv.cause.une <- causality(bv.est, cause = "une") # performing Granger Causality anlaysis for 'une' causing 'gdp'
bv.cause.une


# 8. Impulse Response Analysis
irf.gdp <- irf(bv.est, impulse = "une", response = "gdp", n.ahead = 40, boot = TRUE) # performing IRF for a shock from 'une' to 'gdp'
plot(irf.gdp, ylab = "ouput", main = "Shock from unemployment")
 
irf.une <- irf(bv.est, impulse = "gdp", response = "une", n.ahead = 40, boot = TRUE) # performing IRF for a shock from 'gdp' to 'une'
plot(irf.une, ylab = "unemployment", main = "Shock from output")

irf.une_un <- irf(bv.est, impulse = "une", response = "une", n.ahead = 40, boot = TRUE) # performing IRF for a shock from 'une' to 'une'
plot(irf.une_un, ylab = "unemployment", main = "Shock from unemployment")


# 9. Forecast Error Variance Decomposition
bv.vardec <- fevd(bv.est, n.ahead = 10) # performing FEVD analysis
plot(bv.vardec)


# 10. Forecasting
predictions <- predict(bv.est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "gdp")
plot(predictions, names = "une")

fanchart(predictions, names = "gdp") # creating fancharts
fanchart(predictions, names = "une")