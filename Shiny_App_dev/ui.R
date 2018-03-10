
library(shiny)
library(shinythemes)
library(ggplot2)
library(stringr)
library(plyr)
library(tidyverse)
library(dplyr)
library(magrittr)
library(tools)
library(labelled)
library(gridExtra)
library(ggmap)
library(rsconnect)

#fatality_df_V1<-read.csv("./fatality_df_v1.csv")
fluidPage(fluidRow(
  column(3
         
  ),
  column(9
         
  )
),
theme = shinytheme("darkly"),

titlePanel("US Traffic Fatality Analysis in 2016", windowTitle = "Fatality Analysis"),
sidebarLayout(
  sidebarPanel(
   
    
    checkboxInput("plot1","Nation Wide Fatalities", value=T),
    checkboxInput("plot2","State Wide Fatalities", value=F),
    helpText("Note:Use Select State ",
             "For State Wide Fatalities"),
    hr(),
    selectInput(
      inputId ="causes",
      label="Select Features:",
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
      selected="STATE"
    ),
    hr(),
    selectInput(
      inputId = "color",
      label = "Color By:",
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
    hr(),
    helpText("Note:For Fatal Crashes by Time tab",
             "Use Select Time and Color By"),
    
    selectInput(
      inputId = "time",
      label = "Select Time:",
      choices = c("Hour"="HOUR",
                  "Day of the Week"="DAY_WEEK",
                  "Month"="MONTH"
      ),
      selected="HOUR"
    ),
    
    hr(),    
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
                                 
                                 plotOutput(outputId = "Barplot"),
                                 br(), br()
                                 
                                 # h5("Barplot of charachterstics for States "),
                                 # plotOutput(outputId = "Barplot")
                        ),
                        
                        tabPanel(title="Fatal Crashes by Time",
                                 plotOutput(outputId = "Histogram")),
                        
                        tabPanel(title = "Tennessee",
                                 plotOutput(outputId = "GeoMapping", width ="100%")),
                        
                        tabPanel(title = "Data Source",
                                 htmlOutput(outputId = "Description"))
                        
  )
  
  )
)
)
