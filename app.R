
library(shiny)
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
                    selected ="GENDER"),
        selectInput(inputId = "z",
                    label="Color by:",
                    choices = c("STATE","ST_CASE","FATALS","DEATHS","VEH_NO","AGE","SEX",
                                "RACE","REST_MIS","DAY","MONTH","DAY_WEEK","HOUR","NHS",
                                "RUR_URB","LATITUDE","LONGITUD","WEATHER","FUNC_SYS","MDRDSTRD",
                                "HIT_RUN","TRAV_SP","L_STATE","L_STATUS","SPEEDREL","SPEEDREL",
                                "DR_DRINK","AGE_GROUP","GENDER","Land_Use","SPEED_INVOLVED",
                                "LICENSE_STATUS","WEATHER_CONDITION","DISTRACTED","BMI"),
                    selected="AGE_GROUP")
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
      # generate bins based on input$bins from ui.R
     ggplot(data=fatality_df_V1, aes(input$x)) +
          geom_histogram(breaks=seq(0, 12, by=1), col="yellow",
                          +                  fill="#e2240f", alpha=0.4) + 
          labs(x="Month (Jan-Dec)")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

