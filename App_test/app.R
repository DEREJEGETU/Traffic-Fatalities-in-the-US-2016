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


fatality_df_V1<-read.csv("C:/Users/Dereje/NSSDS/Testing/Traffic-Fatalities-in-the-US-2016/APP_test/fatality_df_V1.csv")
#fatality_df_V1<-read.csv("./fatality_df_v1.csv")
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId ="causes",
        label="casuse_accident;",
        choices = colnames(fatality_df_V1),
        selected="STATE"
      ),
      
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = colnames(fatality_df_V1),
        selected="GENDER"
      ),
      selectInput(
        inputId = "w",
        label = "numeric:",
        choices = colnames(fatality_df_V1),
        selected="HOUR"
      ),
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5)
    ),
    #sidebar panel
    mainPanel(
      plotOutput("Barplot"),
      plotOutput("Histogram")
    )
    
  )
  
)

server <- function(input, output) {
  
  output$Barplot <- renderPlot({ 
    fatality_df_V1 %>%
      ggplot() + 
      geom_bar(
        aes(
          x=fatality_df_V1[[input$causes]], 
          fill=fatality_df_V1[[input$z]]
        )
      )+coord_flip()
  })
  output$Histogram <- renderPlot({ 
    fatality_df_V1 %>%
      ggplot(aes( x=fatality_df_V1[[input$w]])) + 
      geom_histogram(
        aes(
            fill=fatality_df_V1[[input$z]], alpha=input$alpha 
          
        )
      )+labs(x="Hour of Day", y="Number of Accidents")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)