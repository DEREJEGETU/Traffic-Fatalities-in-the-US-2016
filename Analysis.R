

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
library(ggmap)

fatality_df_V1 <- read_csv("App_test/fatality_df_V1.csv")
fatality_df_V1<-fatality_df_V1 %>% 
  select(-c(long))

fatality_df_V1$DR_DRINK<-as_factor(fatality_df_V1$DR_DRINK)

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

install.packages("highcharter")
library(highcharter)
mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))

hcmap("countries/us/us-all", data = fatality_df_V1,
      name = "Fatality Analysis", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 


leaflet(quakes) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 178, -20, 5 ) %>%
  addHeatmap(lng = ~long, lat = ~lat, intensity = ~mag,
             blur = 20, max = 0.05, radius = 15)

TN<-fatality_df_V1 %>% 
  filter(STATE=="Tennessee")

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=unique(TN$LONGITUD), lat=unique(TN$LATITUDE), popup="Accidents")

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>% 
  addMarkers(lng=fatality_df_V1$LONGITUD, lat=fatality_df_V1$LATITUDE, popup="Accidents")

ggplot(fatality_df_V1,aes(x=fatality_df_V1$lng, y=fatality_df_V1$lat))+geom_point()

fatality_df_V1$lng<-fatality_df_V1$LONGITUD

fatality_df_V1$lng[fatality_df_V1$LONGITUD==999.9999] <- NA
fatality_df_V1$lng[fatality_df_V1$LONGITUD==888.88880] <- NA
fatality_df_V1$lng[fatality_df_V1$LONGITUD==777.77770] <- NA


fatality_df_V1$lat<-fatality_df_V1$LATITUDE

fatality_df_V1$lat[fatality_df_V1$lat==99.9999] <- NA
fatality_df_V1$lat[fatality_df_V1$lat==88.88880] <- NA
fatality_df_V1$lat[fatality_df_V1$lat==77.77770] <- NA


write_csv(fatality_df_V1,"C:/Users/Dereje/NSSDS/Testing/Traffic-Fatalities-in-the-US-2016/APP_test/fatality_df_V1.csv", na="NA")

tennessee <- c(lon = -86.5804, lat = 35.5175)
TN_map <- get_map(location = tennessee, zoom = 7,scale = 2, maptype = "roadmap")
TN<-fatality_df_V1%>% 
  filter(STATE=="Tennessee")
ggmap(TN_map,
      base_layer = ggplot(TN,aes(x=lng, y=lat,color=GENDER)))+
  geom_point()

qmplot(lng, lat, data = TN, 
       geom = "point", color = GENDER) 

qmplot(lng, lat, data = fatality_df_V1, 
       geom = "point", color = GENDER) 




ggmap(corvallis_map_bw) +
  geom_tile(aes(lon, lat, fill = predicted_price), 
            data = preds, alpha = 0.8)
  


ggmap(TN_map, 
      base_layer = ggplot(data=fatality_df_V1%>% 
                            filter(STATE=="Tennessee")
      ))+
  geom_point(aes(x=lng, y=lat, fill="GENDER"))+
  title("State of Tennesse Fatality in 2016")