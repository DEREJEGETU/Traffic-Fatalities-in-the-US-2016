
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(plyr)
library(tidyverse)
library(dplyr)
library(magrittr)
library(DT)
library(tools)
library(labelled)
library(gridExtra)
library(maps)
library(leaflet)

write.csv()
fatality_df_V1 <- read_csv("fatality_df_V1.csv")

# Fatality by time 

ggplot(data=fatality_df_V1, aes(fatality_df_V1$HOUR, na.rm=TRUE)) +
  geom_histogram(breaks=seq(0, 24, by=1),col="red",
                 fill="#ffff88", alpha=0.4) + 
  labs(x="Hour of Day", y="Number of Accidents")

ggplot(data=fatality_df_V1, aes(fatality_df_V1$DAY_WEEK)) +
  geom_histogram(breaks=seq(0, 7, by=1), col="yellow",
                 fill="#e2240f", alpha=0.4) + 
  labs(x="Day of Week (Sun-Sat)")

ggplot(data=fatality_df_V1, aes(fatality_df_V1$MONTH)) +
  geom_histogram(breaks=seq(0, 12, by=1), col="yellow",
                 fill="#e2240f", alpha=0.4) + 
  labs(x="Month (Jan-Dec)")

grid.arrange(g1, g2, g3, nrow=2)


###################

  ggplot(fatality_df_V1,na.rm=TRUE) + geom_bar(aes(x=STATE, fill=(AGE_GROUP))) +
    scale_x_discrete(limits = (fatality_df_V1 %>% count(STATE)
                              %>% arrange(n))$STATE) +
    coord_flip() +
    labs(title="Accidents by AGE  GROUP",
         x="State", y="Count", fill="AGEGROUP")
  
  
  ggplot(fatality_df_V1,na.rm=TRUE) + geom_bar(aes(x=STATE,fill=(NHS))) +
    scale_x_discrete(limits = (fatality_df_V1 %>% count(STATE)
                               %>% arrange(n))$STATE) +
    labs(title="Number of Accidents by Offense Type",
         x="Offense Type", y="Count") + coord_flip()
  
  ggplot(fatality_df_V1,na.rm=TRUE) + geom_bar(aes(x=SPEED_INVOLVED))
  #  categorical choice 
  
choices = c("STATE","ST_CASE","FATALS","DEATHS",
              "DAY","MONTH","DAY_WEEK","HOUR","NHS",
              "RUR_URB","LATITUDE","LONGITUD","FUNC_SYS",
              "HIT_RUN","L_STATUS","DR_DRINK","AGE_GROUP",
              "GENDER","Land_Use","SPEED_INVOLVED",
              "LICENSE_STATUS","WEATHER_CONDITION","DISTRACTED","BMI")

fatality_df_V1$long<-fatality_df_V1$LONGITUD
fatality_df_V1$lat<-fatality_df_V1$LATITUDE

p<-fatality_df_V1%>% 
  filter(STATE=="Tennessee") %>% 
ggplot(aes(x =long, y = lat, group = STATE, fill =FATALS)) +
  geom_polygon() +
  coord_equal() +
  
  table(fatality_df_V1$GENDER,fatality_df_V1$Land_Use)

  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Average AGI Per Return")
ggplotly(p)