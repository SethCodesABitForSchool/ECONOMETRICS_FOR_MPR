# Structural Change #

# 1. Install necessary packages
install.packages("remotes") 
remotes::install_github("KevinKotze/tsm") 
install.packages("vars")
library(tsm)
library(vars) 

# 2. Load data
BREAK <- read_excel("DATA/BREAK.xls") 
view(BREAK) 
Y1 <- BREAK$Y1 # extract 'Y1' column
y1 <- ts(Y1) # convert it into a time series object

# 3. Perform unit root test
Y1_u = ur.df(Y1, type = "none", selectlags ="AIC") # test the null hypothesis of unit root
summary(Y1_u) # unable to reject the null of a unit root

# 4-1. Plot the time series
ts.plot(Y1) # structural break at observation 50

# 4-2. The Zivot-Andrews test - Unknown Structural break
Y1_un <- ur.za(Y1, model = "intercept", lag = 0)  # test the potential break occured in the intercept
summary(Y1_un) # structural break at observation 50

# 5. Removing the break and test for unit root
S <- c(rep(0,49), rep(1,51)) # construct a dummy
resids <- (lm(Y1~S-1))$residuals # regress the variable on the dummy
Y1_re <- ur.df(resids, type='none', selectlags = c("AIC")) 
summary(Y1_re) # able to reject the null of a unit root after removing the structural break
