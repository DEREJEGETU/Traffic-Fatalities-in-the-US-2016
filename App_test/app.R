library(shiny)
library(shinythemes)
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
library(ggmap)
library(leaflet)


fatality_df_V1<-read.csv("C:/Users/Dereje/NSSDS/Testing/Traffic-Fatalities-in-the-US-2016/APP_test/fatality_df_V1.csv")
tennessee <- c(lon = -86.5804, lat = 35.5175)
TN_map <- get_map(location = tennessee, zoom = 7,scale = 2, maptype = "roadmap")


#fatality_df_V1<-read.csv("./fatality_df_v1.csv")
ui <- fluidPage(fluidRow(
  column(3
         
  ),
  column(9
         
  )
),
  theme = shinytheme("darkly"),

  titlePanel("US Traffic Fatality Analysis in 2016", windowTitle = "Fatality Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Select for an Exploratory Analysis"),
      checkboxInput("plot1","Make Plot 1", value=T),
      checkboxInput("plot2","Make Plot 2", value=F),
      selectInput(
        inputId ="causes",
        label="characteristics  :",
        choices = c("State"= "STATE",
                    "Gender" = "GENDER",
                    "On Highway"="NHS",
                    "Rural vs Urban"= "Land_Use",
                    "Weather Condition"="WEATHER_CONDITION",
                    "Distraction Involved"="DISTRACTED",
                    "Age Group"="AGE_GROUP",
                    "Drunk Driver"="DR_DRINK",
                    "License Status"="LICENSE_STATUS",
                    "Speed Involved"="SPEED_INVOLVED",
                    "Hit and Run"="HIT_RUN"),
        selected="GENDER"
      ),
      
      selectInput(
        inputId = "color",
        label = "Color by:",
        choices = c("State"= "STATE",
                    "Gender" = "GENDER",
                    "On Highway"="NHS",
                    "Rural vs Urban"= "Land_Use",
                    "Weather Condition"="WEATHER_CONDITION",
                    "Distraction Involved"="DISTRACTED",
                    "Age Group"="AGE_GROUP",
                    "Drunk Driver"="DR_DRINK",
                    "License Status"="LICENSE_STATUS",
                    "Speed Involved"="SPEED_INVOLVED",
                    "Hit and Run"="HIT_RUN"),
        selected="AGE_GROUP"
      ),
      selectInput(
        inputId = "time",
        label = "Time of Accident:",
        choices = c("Hour"="HOUR",
                    "Day of the Week"="DAY_WEEK",
                    "Month"="MONTH"
        ),
        selected="HOUR"
      ),
      
      br(),
      br(),
      
      #sliderInput(inputId = "alpha", 
                  #label = "Alpha:", 
                 # min = 0, max = 1, 
                #  value = 0.5),
 
    #sliderInput(inputId = "bins", 
                #label = "Number of bins:", 
                #min = 0, max = 24, 
                #value = 1),
    #textInput(inputId = "plot_title",
              #label = "Plot title",
             # placeholder = "Enter text to be used as plot title")
    
    selectInput(inputId = "selected_state",
                       label = "Select State:",
                       choices = c("Alabama", "Alaska", "American Samoa","Arizona", "Arkansas", 
                                            "California", "Colorado","Connecticut","Delaware", "District of Columbia", "Florida","Georgia", "Guam", 
                                            "Hawaii", "Idaho","Illinois","Indiana", "Iowa","Kansas","Kentucky", "Louisiana", "Maine","Maryland", "Massachusetts",
                                            "Michigan", "Minnesota","Mississippi","Missouri", "Montanaa", "Nebraska","Nevada", "New Hampshire",
                                            "New Jersey", "New Mexico","New York","North Carolina", "North Dakota","Ohio","Oklahoma", "Oregon", "Pennsylvania","Puerto Rico", "Rhode Island",
                                            "South Carolina", "South Dakota","Tennessee","Texas", "Utah", "Vermont","Virginia", "Virgin Islands","Washington","West Virginia", "Wisconsin","Wyoming"),
    
                       selected = "Tennessee")
  ),
    #Main panel
    mainPanel(tabsetPanel(id = "tabspanel", type = "tabs",
                  tabPanel(title = "Plots",
                           
                           h5("Barplot of charachterstics for US "),
                           plotOutput(outputId = "Barplot"),
                           br(), br()
                  
                          # h5("Barplot of charachterstics for States "),
                          # plotOutput(outputId = "Barplot")
                          ),
                 
                  tabPanel(title="Accident by time",
                            h5("Accidents Distribution by time for US"),
                            plotOutput(outputId = "Histogram")),
                  
                  tabPanel(title = "Visualizing Tennessee",
                          plotOutput(outputId = "GeoMapping", width ="100%")),
                  
                  tabPanel(title = "Data Source",
                           htmlOutput(outputId = "Description"))
                  
      )
    
    )
  )
)



server <- function(input, output) {
  
  state_subset <- reactive({
     a<-subset(fatality_df_V1, STATE==input$selected_state)
     a<-droplevels(a)
  })
  
  pt1<-reactive({
    if(!input$plot1) return(NULL)
    ggplot(data=fatality_df_V1,aes(
      x=fatality_df_V1[[input$causes]], 
      fill=fatality_df_V1[[input$color]]
    )) + 
      geom_bar()+
      coord_flip()+
      labs(x = toTitleCase(paste(input$causes)),
           fill = toTitleCase(input$color),
           title = "Barplot for Accidents")
  })
  
  pt2<-reactive({
    if(!input$plot2) return(NULL)
    ggplot(data=state_subset()
    ) + 
      geom_bar(aes(x=state_subset()[[input$causes]], 
                   fill=state_subset()[[input$color]]))+
      labs(x = toTitleCase(paste(input$causes)),
           fill = toTitleCase(input$color),
           title = "Barplot")
  })
  
  output$Barplot=renderPlot({
    ptlist<-list(pt1(),pt2())
    wtlist<-c(4.5,4.5)
    to_delete<-!sapply(ptlist,is.null)
    ptlist<-ptlist[to_delete]
    wtlist<-wtlist[to_delete]
    if(length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist, widths=wtlist,nocol=length(ptlist))
  })
  
# output$Barplot_US <- renderPlot({ 
#    
#    ggplot(data=fatality_df_V1,aes(
#      x=fatality_df_V1[[input$causes]], 
#      fill=fatality_df_V1[[input$color]]
#    )) + 
#      geom_bar()+
#      coord_flip()+
#      labs(x = toTitleCase(paste(input$causes)),
#           fill = toTitleCase(input$color),
#           title = "Barplot for Accidents")
#  })
#  
#  output$Barplot<- renderPlot({ 
#    
#    ggplot(data=state_subset()
#    ) + 
#      geom_bar(aes(x=state_subset()[[input$causes]], 
#                   fill=state_subset()[[input$color]]))+
#      labs(x = toTitleCase(paste(input$causes)),
#           fill = toTitleCase(input$color),
#           title = "Barplot")
#  })
 
 

 output$Histogram <- renderPlot({ 
   #bins <- seq(min(input$time), max(input$time), length.out = input$bins + 1)
   fatality_df_V1 %>%
     ggplot(aes( x=fatality_df_V1[[input$time]])) + 
     geom_histogram(
       aes(#breaks=input$bins,
         fill=fatality_df_V1[[input$color]]#, alpha=input$alpha 
       )
     )+labs(
       x = toTitleCase(input$time),
       fill = toTitleCase(input$color),
       title = "Histogram",
       alpha="Alpha")
 })
 
  
    
  #output$GeoMapping<- renderPlot({ 
      #ggmap(TN_map, 
          #base_layer = ggplot(data=TN_subset(),aes(x=lng, y=lat, color=TN_subset()[[input$color]])))+
     # geom_point()+
     #labs(title="State of Tennesse Fatality in 2016")
       
 # })
  
  
  output$GeoMapping <- renderPlot({
    
      df <- fatality_df_V1 %>% filter(STATE=="Tennessee")
      color <- input$color
      
      ggmap(
        TN_map
      ) +
      geom_point(data=df, aes_string(x="lng", y="lat", color=color)) +
      labs(title="State of Tennesse Fatality in 2016") 
  })    
      
  output$Description <- renderUI({
    
    HTML(paste0("The National Highway Traffic Safety Administration (NHTSA) has a huge open data source on road traffic fatalities.  
                NHTSA's Fatality Analysis Reporting System (FARS) provides data about fatal crashes involving all types of vehicles. FARS is a census of fatal motor 
                vehicle crashes with a set of data files documenting all qualifying fatalities that occurred within the 50 States, the District of Columbia, 
                and Puerto Rico since 1975. To qualify as a FARS case, the crash had to involve a motor vehicle traveling on a traffic way customarily open to the public,
                and must have resulted in the death of a motorist or a non-motorist within 30 days of the crash. Please go to the 
                (https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars) to access the data set.
 "))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)