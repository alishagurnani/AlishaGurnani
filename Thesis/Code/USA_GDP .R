library(plm)
library(plyr)
library(ggplot2)
library(tidyverse)
library(bea.R)
library(httr)


beakey = 'XXXXXXXXX'

#finding parameter values
paramvals <- beaParamVals(beakey, 'NIPA', 'TableID' )
paramvals <- as.data.frame(paramvals)

#specs needed to grab data from BEA
beaspecs <- list(
    'UserID' = beakey,
    'Method' = 'GetData', 
    'DatasetName' = 'NIPA',
    'TableName' = 'T10106',
    'LineNumber' = '1',
    'Frequency' = 'A', 
    'Year' = '2004,2005,2006,2007,2008,2009,2010,2011,2012,2013',
    'ResultFormat' = 'json'
  )

#getting GDP data
beadata <- beaGet(beaspecs, asWide = FALSE)

beaGDP <- beadata %>% 
  filter(LineDescription == "Gross domestic product")

#descriptive analysis on GDP

summary(beaGDP)

str(beaGDP)

beaGDP$TimePeriod <- as.numeric(as.character(beaGDP$TimePeriod))

str(beaGDP)



beaGDP #should be written out to another csv 

write.csv(beaGDP, "Thesis/Data/Processed/BeaGDP.csv", row.names = FALSE)

#visualize GDP for 2004 to 2013

gdp_plot <- ggplot(beaGDP, aes(x = TimePeriod, y = DataValue)) + #group = 1 lets it know that all the observations are a part of one group but I just changed timeperiod to numeric 
  geom_point() +
  geom_path()+
  labs(
    x = "Time",
    y = "Real GDP in Billions", 
    title = "USA GDP for the years 2006-2013"
  )
                     


