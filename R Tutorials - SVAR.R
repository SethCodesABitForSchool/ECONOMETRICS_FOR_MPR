# R tutorials - Structural VAR #

# 1. Install packages
install.packages("quantmod")
install.packages("dplyr")
install.packages("vars")
library(quantmod)
library(dplyr)
library(vars)


# 2. Data preparation
quarterly <- read_excel("DATA/quarterly.xls") # load data
View (quarterly) 

y <- ts(diff(log(quarterly$RGDP)) * 100, start = c(1960, 1), freq = 4) # output
pi <- ts(diff(log(quarterly$CPI)) * 100, start = c(1960, 1), freq = 4) # inflation
i <- ts(quarterly$Tbill, start = c(1960, 1), freq = 4) # short-term interest rates


# 3. Data visualization
data <- cbind(y, pi, i) # combine data into one matrix
colnames(data) <- c("y", "pi", "i")
plot.ts(data) # plot the times series data

# 4. Lag selection and estimation
data <- data[complete.cases(data), ] # remove any missing values
info.var <- VARselect(data, lag.max = 12, type = "both") # select lag order using VARselect
info.var$selection

var.est1 <- VAR(data, p = 6, type = "const", season = NULL) # estimate VAR model with selected lag order
summary(var.est1)

# 5-1. SVAR Model estimation with Short-term restrictions
# Restrictions: 
  # i) Only shocks to output (y) can shift output (y) contemporaneously. 
  # ii) Only shocks to output (y) and inflation (pi) can shift inflation (pi) contemporaneously. 
  # iii) All shocks (y, pi, i) can affect the interest rate (i) contemporaneously.
a.mat <- diag(3) 
diag(a.mat) <- NA
print(a.mat)
a.mat[2, 1] <- NA # output shock affects inflation contemporaneously
a.mat[3, 1] <- NA # output shock affects interest rate contemporaneously
a.mat[3, 2] <- NA # # inflation shock affects interest rate contemporaneously
print(a.mat)

b.mat <- diag(3)
diag(b.mat) <- NA
print(b.mat)

svar.one <- SVAR(var.est1, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE) # estimate SVAR model with short-term restrictions
svar.one

# Impulse Response Analysis
one.int <- irf(svar.one, response = "i", impulse = "i", n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.int)

one.gdp <- irf(svar.one, response = "y", impulse = "i", n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.gdp)

one.inf <- irf(svar.one, response = "pi", impulse = "i", n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.inf)



# 5-2. SVAR Model estimation with Long-run restrictions
# Prepare data for long-term restrictions
dyt <- ts(diff(log(quarterly$RGDP))*100, start = c(1960, 1), freq = 4) # output
unt <- ts(quarterly$Unemp, start = c(1960, 1), freq = 4) # unemployment rate
dyt <- dyt - mean(dyt) # subtract means
unt <- unt - mean(unt) # subtract means

vardat0 <- cbind(dyt, unt) # combine data into one matrix
plot.ts(vardat0, main = "")

# Select lag order using VARselect
vardat0 <- na.omit(vardat0) # remove any missing values
info.vardat0 <- VARselect(vardat0, lag.max = 12, type = "both")
info.vardat0$selection

# Estimate VAR model with selected lag order
model0 <- VAR(vardat0, p = 4, type = "none") 
summary(model0)

# Apply Blanchard-Quah long-run restrictions
model1 <- BQ(model0)
summary(model1)

# Perform Impulse Response Analysis for output and unemployment shocks
irf.dyt <- irf(model1, impulse = "dyt", boot = FALSE, n.ahead = 40)
irf.unt <- irf(model1, impulse = "unt", boot = FALSE, n.ahead = 40)
supply <- cbind(cumsum(irf.dyt$irf$dyt[, 1]), irf.dyt$irf$dyt[, 2])
demand <- cbind(-1 * cumsum(irf.unt$irf$unt[, 1]), -1 * irf.unt$irf$unt[, 2])

# Impulse Response Analysis
plot.ts(demand[, 1], col = "black", lwd = 2, ylab = "", xlab = "", main = "Demand Shock", xlim = c(0, 40), ylim = c(-0.6, 1.4))
lines(demand[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "topright", c("Output response", "Unemployment response"), col = c("black", "blue"), lwd = 2, bty = "n")

plot.ts(supply[, 1], col = "black", lwd = 2, ylab = "", xlab = "", main = "Supply Shock", xlim = c(0, 40), ylim = c(-0.6, 2))
lines(supply[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "topright", c("Output response", "Unemployment response"), col = c("black", "blue"), lwd = 2, bty = "n")
