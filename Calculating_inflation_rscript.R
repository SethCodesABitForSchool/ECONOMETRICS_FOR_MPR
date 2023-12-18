# Install and load FRED-R package
#install.packages("fredr")
library(fredr)
FRED_API_KEY <- fredr_set_key("your_fred_account_api_key_here")
fredr_has_key()
fredr_get_key()

#retrieve CPI data
CPIdata <- fredr(
  series_id = "CPILFESL",
  observation_start = as.Date("1957-01-01"),
  observation_end = as.Date("2023-8-01"),
  frequency = "m", # m for monthly
  units = "lin" #levels of data, no transformation
)
# view dataset
view(CPIdata)

#drop redundant columns from dataset
CPIdata <- subset(CPIdata, select = -c(series_id, realtime_start, realtime_end))
View(CPIdata)

# Install TS packages
#install.packages("TSstudio")
#install.packages("tidyverse")
#install.packages("zoo")
library("TSstudio")
library("tidyverse")
library("zoo")

# re-label column 2 in dataset
colnames(CPIdata)[2] <-"CPI"
View(CPIdata)

#data manipulation
library("quantmod")
library("dplyr")

cpi_infl = Delt(CPIdata$CPI,k=12,type="arithmetic")

cpi_infl = cpi_infl*100 # inflation rate in %

# declare cpi_infl as TS variable
cpi_infl = ts(cpi_infl,start = c(1958,1,1),frequency=12)

# plot calculated inflation rate
ts_plot(cpi_infl, title="U.S. CPI inflation Year-on-Year (1958m1-2023m8)", color="blue",width=3)
