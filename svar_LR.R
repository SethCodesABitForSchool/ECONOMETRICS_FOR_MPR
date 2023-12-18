# initialize
# clear workspace
rm(list = ls())
graphics.off()
# load packages
library(vars)
# import U.S. productivity and hours (dta) dataset
#OutputHours_1948_2022Q2 <- subset(OutputHours_1948_2022Q2, select = c(quarter, lprod, hours))
#save(OutputHours_1948_2022Q2,file="OutputHours_1948_2022Q2.Rdata")
load("~/R/OutputHours_1948_2022Q2.RData")
View(OutputHours_1948_2022Q2)
# create TS objects
lprod <- ts(OutputHours_1948_2022Q2$lprod,start = c(1948,1), frequency = 4)
hours <- ts(OutputHours_1948_2022Q2$hours,start = c(1948,1), frequency = 4)
# de-mean data
lprod <- lprod-mean(lprod)
hours <- hours-mean(hours)
# bind variables into single object
prod_hours_data <-cbind(lprod, hours)
# visualize data
plot.ts(prod_hours_data,main="")
# create subsample of data (1984-2008)
lprod_gm<-window(lprod,start=c(1984,1),end=c(2007,4))
hours_gm<-window(hours,start=c(1984,1),end=c(2007,4))
# de-mean data
lprod_gm <- lprod_gm-mean(lprod_gm)
hours_gm <- hours_gm-mean(hours_gm)
prod_hours_data_gm <-cbind(lprod_gm, hours_gm)
plot.ts(prod_hours_data_gm,main="")

# Estimate a VAR(4) as in Gali (1999)
model0 <- VAR(prod_hours_data_gm, p = 4, type = "none")
summary(model0)

# Apply Long-run restrictions using the BQ command
model1 <- BQ(model0)
summary(model1)

# Extract standarized IRFs

irf.lprod_gm <- irf(model1, impulse = "lprod_gm", boot = FALSE, n.ahead = 24)
irf.hours_gm <- irf(model1, impulse = "hours_gm", boot = FALSE, n.ahead = 24)

# Create oject with Cumulative IRFs of Technology shocks
tech <- cbind(cumsum(irf.lprod_gm$irf$lprod_gm[, 1]), irf.lprod_gm$irf$lprod_gm[, 2])
# Create oject with Cumulative IRFs of Non-technology shocks
nontech <- cbind(cumsum(irf.hours_gm$irf$hours_gm[, 1]), irf.hours_gm$irf$hours_gm[, 2])

# Plot IRFs

# 1. Technology shock
plot.ts(tech[, 1], col = "black", lwd = 2, ylab = "", 
        xlab = "", main = "Technology Shock", xlim = c(0, 24), ylim = c(-1.0, 3))
lines(tech[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "topright", c("Productivity ", "Hours "), 
       col = c("black", "blue"), lwd = 2, bty = "n")

# 2. Nontechnology shock
plot.ts(nontech[, 1], col = "black", lwd = 2, ylab = "", 
        xlab = "", main = "Non-technology Shock", xlim = c(0, 24), ylim = c(-0.2, 2))
lines(nontech[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "topright", c("Productivity ", "Hours "), 
       col = c("black", "blue"), lwd = 2, bty = "n")