# # Tutorial: Augmented Dickey-Fuller Test #

# 1. Install packages
install.packages("remotes") 
remotes::install_github("KevinKotze/tsm") 
install.packages("vars") 
library(tsm)
library(vars)


# 2. Load the data
RGDP <- read_excel("DATA/RGDP.xls") # Quarterly Real U.S. GDP data
View (RGDP)
rgdp<-ts(RGDP$RGDP,start=c(1947,1,1),frequency=4) 
Lrgdp = log(rgdp) # create a log-transformed variable


# 3. Augmented Dickey-Fuller Test
Lrgdp.ct = ur.df(Lrgdp, type = "trend", selectlags ="AIC") # test for the presence of a unit root including both a drift and linear time trend
summary(Lrgdp.ct) # interpret the result

# 𝚫y_t= a_0 + 𝛄*y_(t-1) + a_2*t + ε_t 
# hypothesis 𝛄 = 0               Test statistics tau3
# hypothesis 𝛄 = a_2 = 0         Test statistics phi3
# hypothesis 𝛄 = a_2 = a_0 = 0   Test statistics phi2

Lrgdp.t = ur.df(Lrgdp, type = "drift", selectlags ="AIC") # test for the presence of a unit root including a drift term
summary(Lrgdp.t) 

Lrgdp.none <- ur.df(Lrgdp, type = "none", selectlags = c("AIC")) # test for the presence of a unit root - pure random walk model
summary(Lrgdp.none)
