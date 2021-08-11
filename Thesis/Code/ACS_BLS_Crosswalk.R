
#ACS/BLS Crosswalks:

library(ipumsr)
library(tidyverse)
library(dplyr)
library(tidycensus)
library(gdata)
library(readxl)


#BRING IN THE CROSSWALK
column_names <- c("Sort_Order", "SOC_ID", "Occupation_Title", "ACS_CODE", "ACS_Occupation_Title")

ACS_Crosswalk <- read_excel("Thesis/R Scripts /nem-occcode-acs-crosswalk.xlsx",
                                sheet= 1,
                                skip = 5,
                                col_names= column_names)

#select only columns needed

ACS_Crosswalk <- ACS_Crosswalk %>%
  select(SOC_ID, ACS_CODE)




#bring in BLS SOC 2010
BLScolumn_names <- c("Occupation_Name", "SOC_ID", "Education", "Work_Experience", "Training")

BLS_2010 <- read_excel("Thesis/Data/Education and Training Classifications 2010-present.xlsx",
                                  sheet= 1,
                                  skip = 2,
                                  col_names= BLScolumn_names)

str(BLS_2010)

table(BLS_2010$Education)
count(BLS_2010)

#Join the ACS and BLS 2010 
AcsBls_Cross2010 <- left_join(ACS_Crosswalk, BLS_2010, by = "SOC_ID")
AcsBls_Cross2010$OCCID<- str_remove(AcsBls_Cross2010$SOC_ID, "[-]")

#ACS/BLS CSV for 2010

write.csv(AcsBls_Cross2010, "Thesis/Data/Processed/ACS_BLS_2010.csv", row.names = FALSE)









