
################################################

#Building overeducation column for 2008 ACS data

################################################

#IPUMS/ACS 2008 data


library(ipumsr)
library(tidyverse)
library(dplyr)
library(tidycensus)
library(gdata)
library(readxl)


#Bring in all IPUMS ACS census data

ddi <- read_ipums_ddi("Thesis/Data/Raw/usa_00007.xml")
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

#Clean ACS_2008 and recode
ACS_2008 <- data_sub %>% 
  filter(YEAR == "2008") %>%
  select(YEAR, US2008A_SCHL,US2008A_SOCP, OCC, OCCSOC, US2008A_OCC3, US2008A_OCC, STATEFIP, AGE, RACE, SEX, MARST, CITIZEN, INCTOT, REGION,IND, EMPSTAT)

table(ACS_2008$YEAR) #to make sure there is only 2009 data

table(ACS_2008$EMPSTAT) #to make sure there is only employed ppl in this 

table(ACS_2008$US2008A_SCHL) # to see what education levels 

head(ACS_2008$US2008A_OCC)
table(ACS_2008$AGE)

count(ACS_2008)

ACS_2008['EducAttainment'] <- NA
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL == "01" | ACS_2008$US2008A_SCHL == "02" | ACS_2008$US2008A_SCHL == "03" | ACS_2008$US2008A_SCHL == "04" | 
                          ACS_2008$US2008A_SCHL == "05" | ACS_2008$US2008A_SCHL == "06" | ACS_2008$US2008A_SCHL == "07" | ACS_2008$US2008A_SCHL == "08"] =1 #Less than HS
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL == "09" |ACS_2008$US2008A_SCHL == 10 | ACS_2008$US2008A_SCHL == 11 | ACS_2008$US2008A_SCHL == 12|
                          ACS_2008$US2008A_SCHL == 13 |  ACS_2008$US2008A_SCHL == 14 | ACS_2008$US2008A_SCHL == 15] = 1 #Less than HS
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL == 16 | ACS_2008$US2008A_SCHL == 17] =  2 #HS/GED
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL == 18 | ACS_2008$US2008A_SCHL == 19]= 3 #Some college no degree
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL == 20] = 4 #Associates
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL== 21] = 5 #Bachelors
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL== 22] = 6 #Master's
ACS_2008$EducAttainment[ACS_2008$US2008A_SCHL== 23 | ACS_2008$US2008A_SCHL== 24] = 7 #Doctorate

table(ACS_2008$EducAttainment)


str(ACS_2008$US2008A_OCC)

#removing leading 0s in 2008A_OCCP
ACS_2008$US2008A_OCC = str_remove(ACS_2008$US2008A_OCC, "^0+")

#bring in occupation SOC crosswalk 2010 data

occupation_educ2010 <- read.csv("Thesis/Data/Processed/ACS_BLS_2010.csv")


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


#TEST RUN FIRST - only 200 rows
ACS_2008_2<- ACS_2008[1:200,] 

ACS_2008_2['Overeducation']<-NA 

for (i in 1:nrow(ACS_2008_2)){
  if (ACS_2008_2$US2008A_SOCP[i] %in% names(SOC_dictionary)){
    if(ACS_2008_2$EducAttainment[i] > SOC_dictionary[[ACS_2008_2$US2008A_SOCP[i]]]) {
      ACS_2008_2$Overeducation[i] <- 1
    }
    else {
      ACS_2008_2$Overeducation[i] <- 0
    }
  }
  else if (ACS_2008_2$US2008A_OCC[i] %in% names(ACS_dictionary)) {
    if(ACS_2008_2$EducAttainment[i] > ACS_dictionary[[ACS_2008_2$US2008A_OCC[i]]]) {
      ACS_2008_2$Overeducation[i] <- 1 
    }
    else{
      ACS_2008_2$Overeducation[i] <- 0
    }
  }
  else {
    ACS_2008_2$Overeducation[i] <- NA
  }
}

print(head(ACS_2008_2[30:40,c(3:6,18:19)]))

data_attempt <- occupation_educ2010 %>%
  filter(OCCID == 493023)

data_attempt <- occupation_educ2010 %>%
  filter(ACS_CODE == 9620)


#BUILD OVEREDUCATION COLUMN 

ACS_2008['Overeducation']<-NA 

for (i in 1:nrow(ACS_2008)){
  if (ACS_2008$US2008A_SOCP[i] %in% names(SOC_dictionary)){
    if(ACS_2008$EducAttainment[i] > SOC_dictionary[[ACS_2008$US2008A_SOCP[i]]]) {
      ACS_2008$Overeducation[i] <- 1
    }
    else {
      ACS_2008$Overeducation[i] <- 0
    }
  }
  else if (ACS_2008$US2008A_OCC[i] %in% names(ACS_dictionary)) {
    if(ACS_2008$EducAttainment[i] > ACS_dictionary[[ACS_2008$US2008A_OCC[i]]]) {
      ACS_2008$Overeducation[i] <- 1 
    }
    else{
      ACS_2008$Overeducation[i] <- 0
    }
  }
  else {
    ACS_2008$Overeducation[i] <- NA
  }
}

print(head(ACS_2008_2[,c(3:7,15:18)]))


# look at data
head(ACS_2008)
tail(ACS_2008)
print(head(ACS_2008[30:35,c(3:6,15:19)]))


#check to see if it worked 
data_attempt <- occupation_educ2010 %>%
  filter(OCCID == 252030)

data_attempt <- occupation_educ2010 %>%
  filter(ACS_CODE == 2320)

#write out to csv
write.csv(ACS_2008, "Thesis/Data/Processed/ACS_2008.csv", row.names = FALSE)

