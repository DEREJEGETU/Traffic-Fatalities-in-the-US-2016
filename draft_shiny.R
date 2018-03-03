library(shiny)
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

attach(fatality_df_V1)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Fatality Analysis by time of Incident",windowTitle = "Time of Incident"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Histogram"),
      selectInput(inputId ="y",
                  label = "Y_Variables:",
                  choices = c("STATE","ST_CASE","FATALS","DEATHS","VEH_NO","AGE","SEX",
                              "RACE","REST_MIS","DAY","MONTH","DAY_WEEK","HOUR","NHS",
                              "RUR_URB","LATITUDE","LONGITUD","WEATHER","FUNC_SYS","MDRDSTRD",
                              "HIT_RUN","TRAV_SP","L_STATE","L_STATUS","SPEEDREL","SPEEDREL",
                              "DR_DRINK","AGE_GROUP","GENDER","Land_Use","SPEED_INVOLVED",
                              "LICENSE_STATUS","WEATHER_CONDITION","DISTRACTED","BMI"),
                  selected ="DAY_WEEK"),
      
      selectInput(inputId = "x",
                  label="X_Variales",
                  choices = c("STATE","ST_CASE","FATALS","DEATHS","VEH_NO","AGE","SEX",
                              "RACE","REST_MIS","DAY","MONTH","DAY_WEEK","HOUR","NHS",
                              "RUR_URB","LATITUDE","LONGITUD","WEATHER","FUNC_SYS","MDRDSTRD",
                              "HIT_RUN","TRAV_SP","L_STATE","L_STATUS","SPEEDREL","SPEEDREL",
                              "DR_DRINK","AGE_GROUP","GENDER","Land_Use","SPEED_INVOLVED",
                              "LICENSE_STATUS","WEATHER_CONDITION","DISTRACTED","BMI"),
                  selected ="HOUR"),
      selectInput(inputId = "z",
                  label="Color by:",
                  choices = c("STATE","ST_CASE","FATALS","DEATHS","VEH_NO","AGE","SEX",
                              "RACE","REST_MIS","DAY","MONTH","DAY_WEEK","HOUR","NHS",
                              "RUR_URB","LATITUDE","LONGITUD","WEATHER","FUNC_SYS","MDRDSTRD",
                              "HIT_RUN","TRAV_SP","L_STATE","L_STATUS","SPEEDREL","SPEEDREL",
                              "DR_DRINK","AGE_GROUP","GENDER","Land_Use","SPEED_INVOLVED",
                              "LICENSE_STATUS","WEATHER_CONDITION","DISTRACTED","BMI"),
                  selected="HOUR")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabspanel",type="tabs",
                  tabPanel(title="Plot",
                           plotOutput(outputId = "hist"))),
      tabPanel(title = "Scatter plot",
               plotOutput(outputId = "scatter"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hist <- renderPlot({
    ggplot(data=fatality_df_V1, aes(input$x, na.rm=TRUE)) +
      geom_histogram(breaks=seq(0, 24, by=1), stat = count(fatality_df_V1$STATE),  col="#e2240f",
                     fill="#ffff88", alpha=0.4) + 
      labs(x="Hour of Day", y="Number of Accidents")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

