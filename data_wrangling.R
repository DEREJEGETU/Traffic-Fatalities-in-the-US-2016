# Data Cleaning 

# Set the working directory 

setwd("C:/Users/Dereje/NSSDS/Testing/Traffic-Fatalities-in-the-US-2016")


## Loading Packages 
library(readr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(magrittr)
library(DT)
library(tools)
library(labelled)
library(maps)
library(ggmap)
library(gridExtra)


## Importing Data Sets 

distract <- read_csv("Data/Distract.csv")
accident <- read_csv("Data/accident.csv")
person <- read_csv("Data/person.csv")
vehicle <- read_csv("Data/vehicle.csv")


# Nameing columns 

acc_name<-c("STATE", "ST_CASE", "PERSONS","DAY","MONTH","YEAR","DAY_WEEK",
            "HOUR", "MINUTE","NHS","RUR_URB", "LATITUDE", "LONGITUD","LGT_COND", "WEATHER", "FATALS", "FUNC_SYS")

per_name<-c("ST_CASE","VEH_NO","AGE","SEX","RACE","REST_MIS","PER_TYP")
vehc_name<-c("ST_CASE","DEATHS","VEH_NO","UNITTYPE","HIT_RUN","BODY_TYP","MAKE","TRAV_SP","L_STATE","L_STATUS","DR_HGT","DR_WGT","SPEEDREL","DR_DRINK")

dis_name<-c("ST_CASE", "VEH_NO", "MDRDSTRD")
# Subsetting the data set with the variables of interest 

accident_2016<-accident %>% 
  select(acc_name)

vehicle_2016<-vehicle %>%
  select(vehc_name)

person_2016<-person %>%
  select(per_name)

distract_2016<-distract %>%
  select(dis_name)
# merging The data sets in to one master fatality data set 

fatality_df<-person_2016 %>% 
  inner_join(accident_2016, by="ST_CASE") %>%
  inner_join(distract_2016, by=c("ST_CASE","VEH_NO")) %>% 
  inner_join(vehicle_2016, by=c("ST_CASE","VEH_NO")) %>% 
  filter(PER_TYP==1) %>%
  select("STATE", "ST_CASE","PER_TYP","FATALS","DEATHS",everything())


# # Changing Numeric values in to character to map the codes , but later we will change them to factor as necessaary
col_names<-c("STATE", "PER_TYP",  "SEX",  "REST_MIS", "DSTATUS" , "AIR_BAG" , "COUNTY" ,  "CITY",
              "NHS",  "RUR_URB" , "LGT_COND", "WEATHER1","WEATHER2", "WEATHER",  "CF1",      "CF2",      "CF3" ,    
"DRUNK_DR", "FUNC_SYS", "MDRDSTRD" ,"UNITTYPE", "HIT_RUN" , "BODY_TYP", "MAKE",  "L_STATE", "L_STATUS", "DR_HGT" , 
"DR_WGT",   "PREV_ACC", "PREV_SUS", "PREV_DWI", "PREV_SPD", "PREV_OTH", "SPEEDREL", "DR_SF1" ,  "DR_SF2", "DR_SF3" ,"DR_SF4" , 
"DR_DRINK")


fatality_df[,col_names] <- lapply(fatality_df[,col_names] , as.character)
str(fatality_df)

# Creating value labels 

fatality_df$STATE<- factor(fatality_df$STATE,
                              levels = c(1,2,3,4,5,6,8,9,10, 11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,
                                         28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,
                                         52,53,54,55,56),
                              labels = c("Alabama", "Alaska", "American Samoa","Arizona", "Arkansas", 
                                         "California", "Colorado","Connecticut","Delaware", "District of Columbia", "Florida","Georgia", "Guam", 
                                         "Hawaii", "Idaho","Illinois","Indiana", "Iowa","Kansas","Kentucky", "Louisiana", "Maine","Maryland", "Massachusetts",
                                         "Michigan", "Minnesota","Mississippi","Missouri", "Montanaa", "Nebraska","Nevada", "New Hampshire",
                                         "New Jersey", "New Mexico","New York","North Carolina", "North Dakota","Ohio","Oklahoma", "Oregon", "Pennsylvania","Puerto Rico", "Rhode Island",
                                         "South Carolina", "South Dakota","Tennessee","Texas", "Utah", "Vermont","Virginia", "Virgin Islands","Washington","West Virginia", "Wisconsin","Wyoming"))

#state_data <- read_csv("state_data.csv")
#state_data<-state_data %>% 
 # select(State_Name,State_No)

#state_data$State_No<-as.character(state_data$State_No)

#map <-setNames(c(state_data$State_Name),c(state_data$State_No))


#fatality_df$STATE<-map[unlist(fatality_df$STATE)]

#SEX
fatality_df$SEX <- factor(fatality_df$SEX,
                    levels = c(1,2,8,9),
                    labels = c("Male", "Female", "Not Reported","Unknown"))
#RACE
#RACE <- read_csv("RACE.csv")
#map <-setNames(c(RACE$Race),c(RACE$Race_ID))
#fatality_df$RACE<-map[unlist(fatality_df$RACE)]

# REST_MIS USe
fatality_df$REST_MIS<- factor(fatality_df$REST_MIS,
                          levels = c(0,1,8),
                          labels = c("No", "Yes", "Not a Motor Vehicle Occupant"))

# Nathional High way system
fatality_df$NHS<- factor(fatality_df$NHS,
                              levels = c(0,1,9),
                              labels = c("Not on NHS", "On NHS","Unknown"))

# Land Use Rural Urban 
fatality_df$RUR_URB<- factor(fatality_df$RUR_URB,
                         levels = c(1,2,6,8,9),
                         labels = c("Rural","Urban","Trafficway Not in State Inventory","Not Reported", "Unknown"))
# Light Condition
fatality_df$LGT_COND<- factor(fatality_df$LGT_COND,
                             levels = c(1,2,3,4,5,6,7,8,9),
                             labels = c("Daylight","Dark - Not Lighted","Dark - Lighted","Dawn", "Dusk", "Dark - Unknown Lighting",
                                        "Other", "Not Reported","Unknown"))

# Weather 
fatality_df$WEATHER<- factor(fatality_df$WEATHER,
                              levels = c(0,1,2,3,4,5,6,7,8,10,11,12,98,99),
                              labels = c("No Additional Atmospheric Conditions","Clear","Rain","Sleet_Hail", "Snow", "Fog_Smog_Smoke",
                                         "Severe Crosswinds", "Blowing Sand_Soil_Dirt","Other","Cloudy","Blowing Snow",
                                         "Freezing Rain or Drizzle","Not Reported","Unknown"))

#Road functional classification

fatality_df$FUNC_SYS<- factor(fatality_df$FUNC_SYS,
                              levels = c(1,2,3,4,5,6,7,96,98,99),
                              labels = c("Interstate","Principal Arterial","Other_Principal Arterial","Minor Arterial", "Minor Collector", "Major Collector", "Local",
                                         "Trafficway Not in State Inventory", "Not Reported","Unknown"))
# Distraction

fatality_df$MDRDSTRD<- factor(fatality_df$MDRDSTRD,
                              levels = c(0,1,3,4,5,6,7,9,10,12,13,14,15,16,17,18,19,92,93,96,97,98,99),
                              labels = c("Not Distracted","Looked But Did Not See","By Other Occupant(s)","By a Moving Object in Vehicle", "While Talking or Listening to Cellular Phone", "While Manipulating Cellular Phone",
                                         "While Adjusting Audio or Climate Controls", "While Using Other Component/Controls Integral to Vehicle","While Using or Reaching For Device/Object Brought Into Vehicle","Distracted by Outside Person, Object or Event",
                                         "Eating or Drinking","Smoking Related","Other Cellular Phone Related","No Driver Present/Unknown if Driver Present","Distraction/Inattention", "Distraction/Careless","Careless/Inattentive","Distraction (Distracted)Details Unknown",
                                         "Inattention (Inattentive), Details Unknown","Not Reported","Lost In Thought/Day Dreaming","Other Distraction","Unknown if Distracted"
                                         ))


# Hit and Run
fatality_df$HIT_RUN<- factor(fatality_df$HIT_RUN,
                         levels = c(0,1,9),
                         labels = c("NO", "Yes","Unknown"))

#fatality_df$HIT_RUN <- labelled_spss(fatality_df$HIT_RUN, c("NO" = 0, "Yes" =1 ), na_values = "Unknown")

# Car Make 


#copy_labels(from, to)

car_make <- read_csv("car_make.csv")

car_make$Make_no<-as.character(car_make$Make_no)
map <-setNames(c(car_make$Make),c(car_make$Make_no))


fatality_df$MAKE<-map[unlist(fatality_df$MAKE)]

# Driving License State

fatality_df$L_STATE<- factor(fatality_df$L_STATE,
                           levels = c(1,2,3,4,5,6,8,9,10, 11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,
                                      28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,
                                      52,53,54,55,56,0,93,94,95,96,97,98,99),
                           labels = c("Alabama", "Alaska", "American Samoa","Arizona", "Arkansas", 
                                      "California", "Colorado","Connecticut","Delaware", "District of Columbia", "Florida","Georgia", "Guam", 
                                      "Hawaii", "Idaho","Illinois","Indiana", "Iowa","Kansas","Kentucky", "Louisiana", "Maine","Maryland", "Massachusetts",
                                      "Michigan", "Minnesota","Mississippi","Missouri", "Montanaa", "Nebraska","Nevada", "New Hampshire",
                                      "New Jersey", "New Mexico","New York","North Carolina", "North Dakota","Ohio","Oklahoma", "Oregon", "Pennsylvania","Puerto Rico", "Rhode Island",
                                      "South Carolina", "South Dakota","Tennessee","Texas", "Utah", "Vermont","Virginia", "Virgin Islands","Washington","West Virginia", "Wisconsin","Wyoming",
                                      "No Driver Present","Indian Nation", "US Government","Canada","Mexico","Other Foreign Country","Not Reported", "Unknown"))


#state_full_data <- read_csv("state_full_data.csv")

#map <-setNames(c(state_full_data$State_Name),c(state_full_data$State_No))

#state_full_data$State_No<-as.character(state_full_data$State_No)

#fatality_df$L_STATE<-map[unlist(fatality_df$L_STATE)]

#License Status

fatality_df$L_STATUS<- factor(fatality_df$L_STATUS,
                              levels = c(0,1,2,3,4,6,7,9),
                              labels = c("Not Licensed", "Suspended", "Revoked","Expired", "Cancelled or Denied", 
                                         "Valid License", "No Driver Present","Unknown License Status"))


#fatality_df$REST_USE<- factor(fatality_df$REST_USE,
fatality_df$SPEEDREL<- factor(fatality_df$SPEEDREL,
levels = c(0,2,3,4,5,8,9),
labels = c("No", "Yes_Racing","Yes, Exceeded Speed Limit", "Yes, Too Fast for Conditions", 
           "Yes, Specifics Unknown", "No Driver Present","Unknown"))


# Drinking Driver 
fatality_df$DR_DRINK<- factor(fatality_df$DR_DRINK,
                             levels = c(0,1),
                             labels = c("NO", "Yes"))

## Data Managment 

# AGE
min(fatality_df$AGE)
max(fatality_df$AGE)
fatality_df$AGE[fatality_df$AGE==999] <- NA
fatality_df$AGE[fatality_df$AGE==998] <- NA

fatality_df<-mutate(fatality_df,
       AGE_GROUP = case_when(
         AGE >= 65 ~ "65+",
         AGE >= 35 ~ "35-64",
         AGE >= 25 ~ "25-34",
         AGE >= 16 ~ "16-24",
         AGE < 16 ~ "0-15"
       )
)  

fatality_df$AGE_GROUP<-as.factor(fatality_df$AGE_GROUP)

#SEX : unknown and notreported will be replaced to NA 
fatality_df$GENDER<-fatality_df$SEX
fatality_df$GENDER[fatality_df$GENDER=="Not Reported"] <- NA
fatality_df$GENDER[fatality_df$GENDER=="Unknown"] <- NA
fatality_df$GENDER<-as.factor(fatality_df$GENDER)
#fatality_df$GENDER <- lapply(fatality_df$SEX, FUN = function(foo) recode(foo, "Unknown = NA ; Not Reported = NA "))

#RURAL_URBAN
fatality_df$Land_Use<-fatality_df$RUR_URB
fatality_df$Land_Use[fatality_df$Land_Use=="Not Reported"] <- NA
fatality_df$Land_Use[fatality_df$Land_Use=="Unknown"] <- NA
fatality_df$Land_Use[fatality_df$Land_Use=="Trafficway Not in State Inventory"] <- NA

# Hour and Minutes 
fatality_df$HOUR[fatality_df$HOUR==99] <- NA
fatality_df$MINUTE[fatality_df$MINUTE==99] <- NA


# SPEED Related
fatality_df$SPEED_INVOLVED <- revalue(fatality_df$SPEEDREL, c("No"="No", "Yes_Racing"="Yes","Yes, Exceeded Speed Limit"="Yes",
                                                              "Yes, Too Fast for Conditions"="Yes","Yes, Specifics Unknown"="Yes", 
                                                              "No Driver Present"="No Driver Present","Unknown"="Unknown"))

# LicenseStatus

fatality_df$LICENSE_STATUS<-revalue(fatality_df$L_STATUS,c("Valid License"="Valid License","Not Licensed"="Invalid License","Suspended"="Invalid License",
                                                           "Revoked"="Invalid License","Expired"="Invalid License","Cancelled or Denied"="Invalid License", 
                                                           "No Driver Present"="No Driver Present","Unknown License Status"="Unknown License Status"))

# WEATHER
fatality_df$WEATHER_CONDITION<-revalue(fatality_df$WEATHER,c("Clear"="Clear","No Additional Atmospheric Conditions"="Clear", "Rain"="Bad Weather","Sleet_Hail"="Bad Weather",
                                                             "Snow"="Bad Weather","Fog_Smog_Smoke"="Bad Weather","Severe Crosswinds"="Bad Weather", 
                                                             "Blowing Sand_Soil_Dirt"="Bad Weather","Other"="Bad Weather","Cloudy"="Bad Weather","Blowing Snow"="Bad Weather",
                                                             "Freezing Rain or Drizzle"="Bad Weather","Not Reported"="NA","Unknown"="Unknown"))


fatality_df$DISTRACTED<-revalue(fatality_df$MDRDSTRD, c("Not Distracted"="Not Distracted","Unknown if Distracted"="Unknown",
                                                        "While Using or Reaching For Device/Object Brought Into Vehicle"="Other Distraction",
                                                        "Distraction (Distracted)Details Unknown"="Other Distraction","Other Cellular Phone Related"="Cellular Phone Related",
                                                        "While Manipulating Cellular Phone"="Cellular Phone Related","Looked But Did Not See"="Other Distraction",
                                                        "Not Reported"="Unknown","Smoking Related"="Other Distraction","While Adjusting Audio or Climate Controls"="Other Distraction",
                                                       "Eating or Drinking"="Other Distraction","By Other Occupant(s)"="Other Distraction","Other Distraction"=" Other Distraction",
                                                      "Distracted by Outside Person, Object or Event"="Other Distraction","Careless/Inattentive"="Other Distraction",
                                                      "Distraction/Inattention"="Other Distraction","Inattention (Inattentive), Details Unknown"="Other Distraction","While Talking or Listening to Cellular Phone"="Cellular Phone Related",
                                                      "While Using Other Component/Controls Integral to Vehicle"="Other Distraction","Lost In Thought/Day Dreaming"="Other Distraction",
                                                     "Distraction/Careless"="Other Distraction","By a Moving Object in Vehicle"="Other Distraction", "No Driver Present/Unknown if Driver Present"="Other Distraction", " Other Distraction"="Other Distraction"
                                                     ))






x<-fatality_df %>% 
  filter(fatality_df$DISTRACTED== " Other Distraction")


fatality_df$lng<-fatality_df$LONGITUD

fatality_df$lng[fatality_df$lng==999.9999] <- NA
fatality_df$lng[fatality_df$lng==888.88880] <- NA
fatality_df$lng[fatality_df$lng==777.77770] <- NA


fatality_df$lat<-fatality_df$LATITUDE
fatality_df$lat[fatality_df$lat==99.9999] <- NA
fatality_df$lat[fatality_df$lat==88.88880] <- NA
fatality_df$lat[fatality_df$lat==77.77770] <- NA


# BMI
fatality_df<-mutate(fatality_df, BMI=round(DR_WGT /(DR_HGT)**2 * 703))

# 

fatality_df<-fatality_df %>% 
  select(-c(PERSONS,LGT_COND,UNITTYPE,long,lat,MAKE,BODY_TYP))


ggplot(fatality_df,aes(x=long, y=lat))+
  geom_point()

#na_values(fatality_df$Land_Use)
fatality_df %>%
  group_by(STATE) %>% 
  summarise(sum(FATALS)) %>% 
  arrange(desc(`sum(FATALS)`))



fatality_df %>% 
  count(STATE) %>% 
  arrange(desc(n))


ggplot(fatality_df) + geom_bar(aes(x=WEATHER_CONDITION)) +
  scale_x_discrete(limits = (fatality_df %>% count(WEATHER_CONDITION)
                             %>% arrange(n))$WEATHER_CONDITION) +
  labs(title="Number of Accidents by STATE",
       x="STATE", y="Count") +
  coord_flip()

# Accidents by GENDER, AGE_CATAGORY, DRINK, SPEEDREL, Land_Use, HIT_RUN
ggplot(fatality_df) + geom_bar(aes(x=STATE, fill=GENDER)) +
  scale_x_discrete(limits = (fatality_df %>% 
                               count(STATE)%>%                   
                               arrange(n))$STATE) +
  coord_flip() +
  labs(title="Accidents by STATE and GENDER",
       x="STATE", y="Count", fill="GENDER")

fat_df<-fatality_df %>% 
  group_by(ST_CASE,STATE)
ggplot(data=subset(fatality_df %>% group_by(STATE,ST_CASE),!is.na(GENDER))) + geom_bar(aes(x=STATE,fill=GENDER))+
  coord_flip()

# Number of Accidents by Licese Status and fill by any category that I want 
ggplot(fatality_df_V1) + geom_bar(aes(x=L_STATUS, fill=GENDER)) +
  scale_x_discrete(limits = (fatality_df_V1 %>% count(L_STATUS)
                             %>% arrange(n))$L_STATUS) +
  labs(title="Number of Accidents",
       x="Offense Type", y="Count") +
  coord_flip()

#Accident by month , week Day and hour 
ggplot(fatality_df) + geom_bar(aes(x=HOUR, fill=DR_DRINK)) 
  

### Accidents by Time 

g1 <- ggplot(data=fatality_df, aes(fatality_df$HOUR, na.rm=TRUE)) +
  geom_histogram(breaks=seq(0, 24, by=1), col="#e2240f",
                 fill="#ffff88", alpha=0.4) + 
  labs(x="Hour of Day", y="Number of Accidents")

g2 <- ggplot(data=fatality_df, aes(fatality_df$DAY_WEEK)) +
  geom_histogram(breaks=seq(0, 7, by=1), col="yellow",
                 fill="#e2240f", alpha=0.4) + 
  labs(x="Day of Week (Sun-Sat)")

g3 <- ggplot(data=fatality_df, aes(fatality_df$MONTH)) +
  geom_histogram(breaks=seq(0, 12, by=1), col="yellow",
                 fill="#e2240f", alpha=0.4) + 
  labs(x="Month (Jan-Dec)")



grid.arrange(g1, g2, g3, nrow=2)

#WEATHER_CONDITION

ggplot(DC) + geom_bar(aes(x=STATE, fill=AGE_GROUP)) +
  scale_x_discrete(limits = (DC %>% count(STATE)
                             %>% arrange(n))$STATE) +
  coord_flip() 

ggplot(fatality_df) + geom_bar(aes(x=STATE, fill=AGE_GROUP)) +
  scale_x_discrete(limits = (fatality_df %>% count(STATE)
                             %>% arrange(n))$STATE) +
  coord_flip() +
  labs(title="Number of Accidents by Weather condition",
       x="Weather condition", y="Count")


ggplot(fatality_df,aes(x=LONGITUD, y=LATITUDE))+
  geom_point()



ggplot(fatality_df, aes(STATE, fill=GENDER)) + 
  geom_bar(stat="identity") + 
  coord_flip() 

#write_csv(fatality_df_V1,"C:/Users/Dereje/NSSDS/Testing/Traffic-Fatalities-in-the-US-2016/APP_test/fatality_df_V1.csv", na="NA")

