
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

function(input, output) {

  
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
           title = "Drivers Involved in Fatal Crashes Features,Nation Wide")
  })
  
  pt2<-reactive({
    if(!input$plot2) return(NULL)
    ggplot(data=state_subset()
    ) + 
      geom_bar(aes(x=state_subset()[[input$causes]], 
                   fill=state_subset()[[input$color]]))+
      labs(x = toTitleCase(paste(input$causes)),
           fill = toTitleCase(input$color),
           title = "Drivers Involved in Fatal Crashes Features by State")
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
        title = "Drivers Involved in Fatal Crashes Features by time")
    #alpha="Alpha") 
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
      labs(title="Drivers Involved in Fatal Crashes in Tennessee") 
  })    
  
  output$Description <- renderUI({
    
    HTML(paste0("The National Highway Traffic Safety Administration (NHTSA) has a huge open data source on road traffic fatalities.  
                NHTSA's Fatality Analysis Reporting System (FARS) provides data about fatal crashes involving all types of vehicles. FARS is a census of fatal motor 
                vehicle crashes with a set of data files documenting all qualifying fatalities that occurred within the 50 States, the District of Columbia, 
                and Puerto Rico since 1975. To qualify as a FARS case, the crash had to involve a motor vehicle traveling on a traffic way customarily open to the public,
                and must have resulted in the death of a motorist or a non-motorist within 30 days of the crash. Please go to 
                (https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars) to access the data set."
                ))
        
  })
  }

