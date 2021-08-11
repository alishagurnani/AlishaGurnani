
################################################

#Building overeducation column for 2005 ACS data

################################################

#IPUMS/ACS 2005 data


library(ipumsr)
library(tidyverse)
library(dplyr)
library(tidycensus)
library(gdata)
library(readxl)


#Bring in all IPUMS ACS census data

ddi <- read_ipums_ddi("/Users/agurnani/Documents/Spring Semester/Thesis/Data/usa_00008.xml")
data <- read_ipums_micro(ddi, verbose = FALSE)

head(data)

table(data$OCCSOC)
table(data$EMPSTAT)
count(data)

#drop rows with no occupation listed and keep only ppl that are currently employed
data_sub <- subset(data, data$OCCSOC !=0 & data$EMPSTAT ==1)


table(data_sub$EMPSTAT)
table(data_sub$OCCSOC)
table(data_sub$AGE)
head(data_sub)


#Clean ACS_2005 and recode
ACS_2005 <- data_sub %>% 
  filter(YEAR == "2005") %>%
  select(YEAR, US2005A_SCHL,US2005A_SOCP, OCC, OCCSOC, US2005A_OCC3, US2005A_OCCP, STATEFIP, AGE, RACE, SEX, MARST, CITIZEN, INCTOT, REGION,IND, EMPSTAT)

table(ACS_2005$YEAR) #to make sure there is only 2005 data
table(ACS_2005$EMPSTAT)
table(ACS_2005$US2005A_SCHL)

table(ACS_2005$AGE)
count(ACS_2005)

ACS_2005['EducAttainment'] <- NA
ACS_2005$EducAttainment[ACS_2005$US2005A_SCHL == "01" | ACS_2005$US2005A_SCHL == "02" | ACS_2005$US2005A_SCHL == "03" | ACS_2005$US2005A_SCHL == "04" | 
                          ACS_2005$US2005A_SCHL == "05" | ACS_2005$US2005A_SCHL == "06" | ACS_2005$US2005A_SCHL == "07" | ACS_2005$US2005A_SCHL == "08"] = 1 #"No formal educational credential"
ACS_2005$EducAttainment[ACS_2005$US2005A_SCHL == "09"] = 2 #"High school diploma or equivalent"
ACS_2005$EducAttainment[ACS_2005$US2005A_SCHL == "10" | ACS_2005$US2005A_SCHL == "11" ] = 3 # "Some college, no degree"
ACS_2005$EducAttainment[ACS_2005$US2005A_SCHL == "12"] = 4 # "Associate's degree"
ACS_2005$EducAttainment[ACS_2005$US2005A_SCHL== "13"] = 5 #"Bachelor's degree"
ACS_2005$EducAttainment[ACS_2005$US2005A_SCHL== "14"] = 6 #"Master's degree"
ACS_2005$EducAttainment[ACS_2005$US2005A_SCHL== "15" | ACS_2005$US2005A_SCHL== "16"] = 7 #"Doctoral or professional degree"

table(ACS_2005$EducAttainment)


str(ACS_2005$US2005A_OCCP)

#removing leading 0s in 2005A_OCCP
ACS_2005$US2005A_OCCP = str_remove(ACS_2005$US2005A_OCCP, "^0+")


#bring in occupation SOC crosswalk 2010 data
occupation_educ2010 <- read.csv("/Users/agurnani/Documents/Spring Semester/Thesis/Data/Processed/ACS_BLS_2010.csv")


str(occupation_educ2010)

table(occupation_educ2010$Education)
count(occupation_educ2010)



#Recoding Occupation/education data to number levels to make comparison easier
occupation_educ2010["EducID"] <-NA
occupation_educ2010$EducID[occupation_educ2010$Education == "Postsecondary nondegree award"] = 0 #there is no post secondary nondegre award category in the ACS so took it out for now 
occupation_educ2010$EducID[occupation_educ2010$Education == "Less than high school"] = 1
occupation_educ2010$EducID[occupation_educ2010$Education == "High school diploma or equivalent"] = 2
occupation_educ2010$EducID[occupation_educ2010$Education == "Some college, no degree"] = 3
occupation_educ2010$EducID[occupation_educ2010$Education == "Associate's degree"] = 4
occupation_educ2010$EducID[occupation_educ2010$Education== "Bachelor's degree"] = 5
occupation_educ2010$EducID[occupation_educ2010$Education== "Master's degree"] = 6
occupation_educ2010$EducID[occupation_educ2010$Education== "Doctoral or professional degree"] = 7

table(occupation_educ2010$EducID)

#removing - in occupation id 
occupation_educ2010$OCCID <-NA
occupation_educ2010$OCCID <- str_remove(occupation_educ2010$SOC_ID, "[-]")
occupation_educ2010$OCCID

head(occupation_educ2010)

#Creating overeducation binary variable

str(occupation_educ2010)

#Remove NAS from selected variables
ACS_Code_data <- occupation_educ2010 %>%
  select(EducID, ACS_CODE) %>%
  na.omit()

BLS_Code_data <- occupation_educ2010 %>%
  select(EducID, OCCID) %>%
  na.omit()


#create dictionary two with ACS code:

M <- as.list(ACS_Code_data$EducID)
names(M) <- ACS_Code_data$ACS_CODE
ACS_dictionary <- list2env(M, hash = TRUE)

#create dictionary one with SOC code:
L <- as.list(BLS_Code_data$EducID)
names(L) <- BLS_Code_data$OCCID
SOC_dictionary <- list2env(L, hash = TRUE)




#TEST RUN FIRST - BUILD OVEREDUCATION COLUMN --- This one is a test round only 200 rows
ACS_2005_2<- ACS_2005[1:200,] 

ACS_2005_2['Overeducation']<-NA 

for (i in 1:nrow(ACS_2005_2)){
  if (ACS_2005_2$US2005A_SOCP[i] %in% names(SOC_dictionary)){
    if(ACS_2005_2$EducAttainment[i] > SOC_dictionary[[ACS_2005_2$US2005A_SOCP[i]]]) {
      ACS_2005_2$Overeducation[i] <- 1
    }
    else {
      ACS_2005_2$Overeducation[i] <- 0
    }
  }
  else if (ACS_2005_2$US2005A_OCCP[i] %in% names(ACS_dictionary)) {
    if(ACS_2005_2$EducAttainment[i] > ACS_dictionary[[ACS_2005_2$US2005A_OCCP[i]]]) {
      ACS_2005_2$Overeducation[i] <- 1 
    }
    else{
      ACS_2005_2$Overeducation[i] <- 0
    }
  }
  else {
    ACS_2005_2$Overeducation[i] <- NA
  }
}

print(head(ACS_2005_2[,c(3:7,15:18)]))


#BUILD OVEREDUCATION COLUMN 

ACS_2005['Overeducation']<-NA 

for (i in 1:nrow(ACS_2005)){
  if (ACS_2005$US2005A_SOCP[i] %in% names(SOC_dictionary)){
    if(ACS_2005$EducAttainment[i] > SOC_dictionary[[ACS_2005$US2005A_SOCP[i]]]) {
      ACS_2005$Overeducation[i] <- 1
    }
    else {
      ACS_2005$Overeducation[i] <- 0
    }
  }
  else if (ACS_2005$US2005A_OCCP[i] %in% names(ACS_dictionary)) {
    if(ACS_2005$EducAttainment[i] > ACS_dictionary[[ACS_2005$US2005A_OCCP[i]]]) {
      ACS_2005$Overeducation[i] <- 1 
    }
    else{
      ACS_2005$Overeducation[i] <- 0
    }
  }
  else {
    ACS_2005$Overeducation[i] <- NA
  }
}

#look at data
head(ACS_2005)
tail(ACS_2005)
print(head(ACS_2005_2[30:35,c(3:7,15:18)]))


#check to see if it worked 
data_attempt <- occupation_educ2010 %>%
  filter(OCCID == 252030)

data_attempt <- occupation_educ2010 %>%
  filter(ACS_CODE == 2320)

#write out to csv
write.csv(ACS_2005, "/Users/agurnani/Documents/Spring Semester/Thesis/Data/Processed/ACS_2005.csv", row.names = FALSE)




