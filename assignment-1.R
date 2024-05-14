# Assignment #1

###########################################################################
# Question 1
###########################################################################

# In the economy there is a content creator: Viral-Videos; a company dedicated 
# to advertising: Sterling Cooper; and a manufacturing company: Widgeco. 
# Consider the following three scenarios:
#
# (A) Viral-Videos sells $200 of videos directly to consumers; Sterling Cooper 
# sells $200 of advertising services to Widgeco (mostly billboards) and Widgeco 
# sells $800 of widgets to consumers. What is the value added by each company? 
# What is GDP?
#
answer_1_a = "
  The value added by Viral Videos is $200 (videos directly to consumers).
  The value added by Sterling Cooper is unknown...?  We don't know the cost
  it took to produce the services.  We only know the price at which the 
  services were sold.
  The value added by Widgeco is $800 (widgets to consumers).
  So, GDP is $200 + $800 = $1000.
"
#
# (B) Viral-Videos sells $100 of videos directly to consumers, the remainder 
# worth $100 are sold to the Sterling Cooper; Sterling Cooper sells $300 of 
# advertising services to Widgeco (billboards and ads in viral videos) and 
# Widgeco sells $800 of widgets to consumers. What is the value added by each 
# company? What is GDP?
#
answer_1_b = "
  The value added by Viral-Videos is $100...
"
#
# (C) Viral-Videos goes bust and viral videos are now produced by Laurence in 
# his spare time. Laurence uploads the videos to the now rebranded website 
# Sterling Cooper Tube. Sterling Cooper Tube sells $300 of advertising services 
# to Widgeco. Widgeco sells $800 of widgets to consumers. What is the value 
# added by each company? What is GDP?
#
answer_1_c = "
  The value added by Viral-Videos is N/A (went bust).
  The value added by Sterling Cooper Tube is $300, less the cost of producing
  those services (unknown).
  The value added by Widgeco is $800, less the cost of producing its widgets.
  So, GDP is ($300 - unknown) + ($800 - unknown) = unknown.
"
#
###########################################################################
# Question 2
###########################################################################
#--------------------------------------------------------------------------
# The following script is provided as a help for Assignment 1 - Question 2
# The goal is to show how to download data from the World Bank
# This version: May 6, 2024
#--------------------------------------------------------------------------
rm(list=ls())   # Clears the workspace

#--------------------------------------------------------------------------
# Step 1: load libraries
#--------------------------------------------------------------------------
#If you have not done so already install the WDI package 
install.packages('WDI')
install.packages("tidyverse")
library(tidyverse)
library(WDI)
library(ggplot2)
library(dplyr)

#--------------------------------------------------------------------------
# Step 2: download and process data
#--------------------------------------------------------------------------
# Note:for multiple time series add indicator separated by comma:
# c("NY.GDP.PCAP.PP.CD","Indicator 2")

dat = WDI(
  indicator=c("NY.GDP.PCAP.PP.CD"), 
  country=c(
    "all"
  ), 
  start=1960, 
  end=2023,
  extra = TRUE,
  cache = NULL
)

#  Note: if the API for downloading data does not work, download the entire dataset (link in Assignment text) then use the following
# library(readxl) then  dat_load <- read_excel("WDIEXCEL.xlsx")

# Drop aggregates (for example European union) to get a country-level analysis
dat <- subset(
  dat, 
  region!="Aggregates"
)

#--------------------------------------------------------------------------
# Step 3: Visualize Data
#--------------------------------------------------------------------------

dat = WDI(
  indicator=c("NE.CON.PRVT.ZS"), 
  country=c("US", "CN", "DE"),
  extra = FALSE,
  cache = NULL
)
p <- ggplot(
  dat,
  aes(
    x=year,
    y=NE.CON.PRVT.ZS,
    colour=as.factor(country),
    group=country)
) + 
  geom_line() + 
  ggtitle("Consumption as a Percentage of GDP") +
  xlab("Year") +
  ylab("% C/Y (consumption as a percentage of GDP)")

p + labs(fill = "Country")

# Explain what you see. In a paragraph (or two) explain how the three countries 
# differ in terms of C/Y and how this variable is changing over time for the 
# three countries. Provide a conjecture on what might be happening (don’t worry 
# about being wrong) to explain the patterns you see both across countries and 
# over time.

answer_2 = "
  Based on the time-series plot, it appears that (generally speaking) 
  consumption as a percentage of GDP increases over time for the United States, 
  but decreases over time for China and Germany.  That said, China in particular 
  experienced major fluctuations in consumption as a percentage of GDP over 
  time.  For example, from 1960 to approximately 1962, China's C/Y value 
  increased about 50%, before falling dramatically.  The US and Germany also 
  experienced some fluctuations over time, but none of their changes in C/Y 
  compared to the magnitude of China's in the early 1960s.
  
  As to why consumption as a percentage of GDP might be happening, political 
  tensions and/or changes in these countries may provide some clues.  For 
  example, China's agrarian-to-industrial shift during the 'Great Leap Forward' 
  reached it's peak in the early 1960s, which may explain the massive increase 
  in C/Y during that time.  Unfortunately, the subsequent decrease in China's 
  C/Y may have been caused from the famine and economic fallout from the 'Great 
  Leap Forward'.  Meanwhile, the global recession in and around 2008 likely 
  spurred the decline the C/Y for all three countries during that timeframe.  
  The similar declines in 2020 for all three countries could likely be 
  attributed to Covid-19.
"

###########################################################################
# Question 3
###########################################################################

# this is done mostly in Excel

###########################################################################
# Question 4
###########################################################################
#--------------------------------------------------------------------------
#The following script is provided as a help for Assignment 1 - Question 4
#The script highlights how to retrieve automatically data from FRED and perform
#an HP-Filter decomposition
#This version: May 6, 2024
#--------------------------------------------------------------------------
rm(list=ls())   #Clears the workspace

#--------------------------------------------------------------------------
# Step 1: load libraries & Install packages
#--------------------------------------------------------------------------
#install.packages("fredr") #Uncomment this line if you have not installed the fred package before
#If you have issues installing the package you can download it here: https://cran.r-project.org/src/contrib/Archive/fredr/
#Then select install from package archive file option in RStudio

#install.packages("mFilter") #Uncomment this line if you have not installed the mFilter package before

library(fredr) #Access FRED, the Federal Reserve Economic Data
#This is the API key I have requested for this course if you plan to use this script in the long run
#make sure to request your own key for access to the database
fredr_set_key("2bb36d96a51186fcedd3e640135c3699") 

library(mFilter) #Package to perform the HP Filter

#--------------------------------------------------------------------------
# Step 1: Load Data
#--------------------------------------------------------------------------

# Real Gross Domestic Product [GDPC1]
RGDP_DAT <- fredr(
  series_id = "GDPC1", 
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2019-12-31")
)
# Retail Sales: Total (Excluding Food Services) [RSXFS]
RSXFS_DAT <- fredr(
  series_id = "RSXFS", 
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2019-12-31")
)
# E-Commerce Retail Sales [ECOMSA]
ECOMSA_DAT <- fredr(
  series_id = "ECOMSA", 
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2019-12-31")
)

#--------------------------------------------------------------------------
# Step 2: HP Filtering
#--------------------------------------------------------------------------
# Format the series as quarterly time series objects, starting at the first date
# If you modify the starting date above, make sure to modify it below
rgdp_ts<-ts(
  RGDP_DAT$value,
  frequency=4,
  start=c(2000,1),
  end=c(2019,4),
  names="RealGDP"
)
rsxfs_ts<-ts(
  RSXFS_DAT$value,
  frequency=4,
  start=c(2000,1),
  end=c(2019,4),
  names="RetailSales"
) 
ecomsa_ts<-ts(
  ECOMSA_DAT$value,
  frequency=4,
  start=c(2000,1),
  end=c(2019,4),
  names="eCommerceSales"
) 

# Transforming to log
l_rgdp_ts <-log(rgdp_ts)
l_rsxfs_ts <-log(rsxfs_ts)
l_ecomsa_ts <-log(ecomsa_ts)

# Computing Hp-Filter
# Note the hpfilter generated object contains both the cycle and trend variables
hp_gdp <- hpfilter(l_rgdp_ts)
hp_rsxfs <- hpfilter(l_rsxfs_ts)
hp_ecomsa <- hpfilter(l_ecomsa_ts)

#Assigning cycle and trend
cycle_gdp <- hp_gdp$cycle
trend_gdp <- hp_gdp$trend

cycle_rsxfs <- hp_rsxfs$cycle
trend_rsxfs <- hp_rsxfs$trend

cycle_ecomsa <- hp_ecomsa$cycle
trend_ecomsa <- hp_ecomsa$trend

#--------------------------------------------------------------------------
# Step 3: Plotting
#--------------------------------------------------------------------------

# Plotting Trend + Data
plot(l_rgdp_ts, main = "GDP Time Series Trend Over Time", col="red",pch=16, xlab = "Year", ylab = "Real GDP Log",lwd=2)
# adds the trend line
lines(trend_gdp,col="green",lwd=2)
grid (10,10, lty = 3)
legend("topleft", legend=c("Data", "HP Filter Trend Line"),col=c("red", "green"), lty=1:2)

plot(l_rsxfs_ts, main = "Retail Sales Time Series Trend Over Time", col="red",pch=16, xlab = "Year", ylab = "Retail Sales Log",lwd=2)
# adds the trend line
lines(trend_rsxfs,col="green",lwd=2)
grid (10,10, lty = 3)
legend("topleft", legend=c("Data", "HP Filter Trend"),col=c("red", "green"), lty=1:2)

plot(l_ecomsa_ts, main = "E-Commerce Time Series Trend Over Time", col="red",pch=16, xlab = "Year", ylab = "E-Commerce Log", lwd=2)
# adds the trend line
lines(trend_ecomsa,col="green",lwd=2)
grid (10,10, lty = 3)
legend("topleft", legend=c("Data", "HP Filter Trend"),col=c("red", "green"), lty=1:2)



# Plotting The Cycle
ts.plot(
  cycle_gdp, 
  main = "GDP Cyclical Component Trend Over Time", 
  col="red",
  xlab = "Year", 
  ylab = "Difference Between the Data and the Trend Line"
)
abline(h=0, col="blue")

ts.plot(
  cycle_rsxfs, 
  main = "Retail Sales Cyclical Component Trend Over Time", 
  col="red",
  xlab = "Year", 
  ylab = "Difference Between the Data and the Trend Line"
)
abline(h=0, col="blue")

ts.plot(
  cycle_ecomsa, 
  main = "E-Commerce Sales Cyclical Component Trend Over Time", 
  col="red",
  xlab = "Year", 
  ylab = "Difference Between the Data and the Trend Line"
)
abline(h=0, col="blue")


#--------------------------------------------------------------------------
# Step 4: Compute Correlations
#--------------------------------------------------------------------------

# Compute the correlation of the cyclical component of Retail and E-Commerce 
# with the cyclical component of GDP

cor(cycle_gdp, cycle_rsxfs) # this is 0.07888894 = low correlation
cor(cycle_gdp, cycle_ecomsa) # this is 0.8030772 = high correlation

var(cycle_rsxfs) # this is 0.00008351082
var(cycle_ecomsa) # this is 0.001610942

