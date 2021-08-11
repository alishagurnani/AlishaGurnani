
################################################

#Building overeducation column for 2011 ACS data

################################################

##IPUMS/ACS 2011 data


library(ipumsr)
library(tidyverse)
library(dplyr)
library(tidycensus)
library(gdata)
library(readxl)


#Bring in all IPUMS ACS census data

ddi <- read_ipums_ddi("/Users/agurnani/Documents/Spring Semester/Thesis/Data/usa_00007.xml")
data <- read_ipums_micro(ddi, verbose = FALSE)

head(data)

table(data$OCCSOC)
table(data$EMPSTAT)
count(data)

#drop rows with no occupation listed  and keep only ppl that are currently employed
data_sub <- subset(data, data$OCCSOC !=0 & data$EMPSTAT ==1)

table(data_sub$EMPSTAT)


table(data_sub$EMPSTAT)
table(data_sub$OCCSOC)
table(data_sub$AGE)
head(data_sub)


#ACS_2011 Data cleaning
ACS_2011 <- data_sub %>% 
  filter(YEAR == "2011") %>%
  select(YEAR, US2011A_SCHL,US2011A_SOCP, OCC, OCCSOC, US2011A_OCC3, US2011A_OCCP, STATEFIP, AGE, RACE, SEX, MARST, CITIZEN, INCTOT, REGION,IND)

table(ACS_2011$YEAR) #to make sure there is only 2011 data

table(ACS_2011$US2011A_SCHL)

table(ACS_2011$AGE)
count(ACS_2011)


ACS_2011['EducAttainment'] <- NA
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL == "01" | ACS_2011$US2011A_SCHL == "02" | ACS_2011$US2011A_SCHL == "03" | ACS_2011$US2011A_SCHL == "04" | 
                          ACS_2011$US2011A_SCHL == "05" | ACS_2011$US2011A_SCHL== "06" | ACS_2011$US2011A_SCHL == "07" | ACS_2011$US2011A_SCHL == "08"] ="No formal educational credential"
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL == "09" |ACS_2011$US2011A_SCHL == "10" | ACS_2011$US2011A_SCHL== "11" | ACS_2011$US2011A_SCHL == "12"|
                          ACS_2011$US2011A_SCHL == "13" |  ACS_2011$US2011A_SCHL == "14" | ACS_2011$US2011A_SCHL == "15"] = "No formal educational credential"
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL == "16" | ACS_2011$US2011A_SCHL == "17"] = "High school diploma or equivalent"
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL == "18" | ACS_2011$US2011A_SCHL == "19"] = "Some college, no degree"
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL == "20"] = "Associate's degree"
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL== "21"] = "Bachelor's degree"
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL== "22"] = "Master's degree"
ACS_2011$EducAttainment[ACS_2011$US2011A_SCHL== "23" | ACS_2011$US2011A_SCHL== "24"] = "Doctoral or professional degree"

table(ACS_2011$EducAttainment)



str(ACS_2011$US2011A_OCCP)

#removing leading 0s in 2011A_OCCP
ACS_2011$US2011A_OCCP = str_remove(ACS_2011$US2011A_OCCP, "^0+")


#bring in occupation SOC crosswalk 2011 data

occupation_educ2010 <- read.csv("/Users/agurnani/Documents/Spring Semester/Thesis/Data/Processed/ACS_BLS_2010.csv")


str(occupation_educ2010)

table(occupation_educ2010$Education)
count(occupation_educ2010)



#Recoding Occupation/education data to number levels to make comparison easier
occupation_educ2010["EducID"] <-NA
occupation_educ2010$EducID[occupation_educ2010$Education == "Postsecondary nondegree award"] = 0 #there is no post secondary nondegre award category in the ACS so took it out for now 
occupation_educ2010$EducID[occupation_educ2010$Education == "No formal educational credential"] = 1
occupation_educ2010$EducID[occupation_educ2010$Education == "High school diploma or equivalent"] = 2
occupation_educ2010$EducID[occupation_educ2010$Education == "Some college, no degree"] = 3
occupation_educ2010$EducID[occupation_educ2010$Education == "Associate's degree"] = 4
occupation_educ2010$EducID[occupation_educ2010$Education== "Bachelor's degree"] = 5
occupation_educ2010$EducID[occupation_educ2010$Education== "Master's degree"] = 6
occupation_educ2010$EducID[occupation_educ2010$Education== "Doctoral or professional degree"] = 7


#removing - in occupation id 
occupation_educ2010$OCCID2 <-NA
occupation_educ2010$OCCID2<- str_remove(occupation_educ2010$Occupation_ID, "[-]")
occupation_educ2010$OCCID2

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
ACS_2011_2<- ACS_2011[1:200,] 

ACS_2011_2['Overeducation']<-NA 

for (i in 1:nrow(ACS_2011_2)){
  if (ACS_2011_2$US2011A_SOCP[i] %in% names(SOC_dictionary)){
    if(ACS_2011_2$EducAttainment[i] > SOC_dictionary[[ACS_2011_2$US2011A_SOCP[i]]]) {
      ACS_2011_2$Overeducation[i] <- 1
    }
    else {
      ACS_2011_2$Overeducation[i] <- 0
    }
  }
  else if (ACS_2011_2$US2011A_OCCP[i] %in% names(ACS_dictionary)) {
    if(ACS_2011_2$EducAttainment[i] > ACS_dictionary[[ACS_2011_2$US2011A_OCCP[i]]]) {
      ACS_2011_2$Overeducation[i] <- 1 
    }
    else{
      ACS_2011_2$Overeducation[i] <- 0
    }
  }
  else {
    ACS_2011_2$Overeducation[i] <- NA
  }
}

print(head(ACS_2011_2[,c(3:7,15:18)]))


#BUILD OVEREDUCATION COLUMN 

ACS_2011['Overeducation']<-NA 

for (i in 1:nrow(ACS_2011)){
  if (ACS_2011$US2011A_SOCP[i] %in% names(SOC_dictionary)){
    if(ACS_2011$EducAttainment[i] > SOC_dictionary[[ACS_2011$US2011A_SOCP[i]]]) {
      ACS_2011$Overeducation[i] <- 1
    }
    else {
      ACS_2011$Overeducation[i] <- 0
    }
  }
  else if (ACS_2011$US2011A_OCCP[i] %in% names(ACS_dictionary)) {
    if(ACS_2011$EducAttainment[i] > ACS_dictionary[[ACS_2011$US2011A_OCCP[i]]]) {
      ACS_2011$Overeducation[i] <- 1 
    }
    else{
      ACS_2011$Overeducation[i] <- 0
    }
  }
  else {
    ACS_2011$Overeducation[i] <- NA
  }
}

# look at data
head(ACS_2011)
tail(ACS_2011)
print(head(ACS_2011[30:35,c(3:7,15:18)]))


#check to see if it worked 
data_attempt <- occupation_educ2010 %>%
  filter(OCCID == 252030)

data_attempt <- occupation_educ2010 %>%
  filter(ACS_CODE == 2320)


#write out to csv
write.csv(ACS_2011, "/Users/agurnani/Documents/Spring Semester/Thesis/Data/Processed/ACS_2011.csv")

