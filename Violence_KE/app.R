#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    # Home panel
    tabPanel('Home',
             titlePanel('VIOLENCE IN KENYA'),
             hr(),
             br(),
               # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  sliderInput(
                    inputId = "Years",
                    label = "Years Desired :",
                    min = min(year_kills$YEAR),
                    max = max(year_kills$YEAR),
                    step = 5,
                    value = 2015),
                 ),
                 # Show a plot of the years
                 fluidRow(
                   h2('Killings Per year'),
                   br(),
                   # Line graph output
                   plotOutput(outputId = 'year_kills_plot' )
                 )
               ),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "County_DD",
                   label = "County :",
                   choices = unique(df2$ADMIN1)
                   ),
               ),
               # Show a plot of the years
               fluidRow(
                 h2('Most Common type of Violence :'),
                 br(),
                 # Line graph output
                 plotOutput(outputId = 'county_plot' )
               )
             ),

             ),
    tabPanel('Counties',
             titlePanel('COUNTIES IN KENYA MOST AFFECTED : '),
             hr(),
             plotOutput(outputId = 'top10'),
             # barplot(top10_counties$FATALITIES, names.arg = top10_counties$ADMIN1, col = 'red')
             br(),
             br(),
             br(),
             br(),
             titlePanel('COUNTIES IN KENYA LEAST AFFECTED : '),
             hr(),
             plotOutput(outputId = 'bottom10'),
             
             
             
             
             ),
    # tabPanel('Map'),
    
    tabPanel('Predictions',
             titlePanel('IS YOUR COUNTY SAFE ? ...'),
             hr(),
             h3('Enter some random details to find out : '),
             hr(),
             selectInput(
               inputId = 'county_input_DD',
               label = 'Enter your County : ',
               choices = unique(df2$ADMIN1)
             ),
             br(),
             numericInput(
               inputId = 'year_input',
               value = 2022,
               'What Year ? : '
             ),
             br(),
             selectInput(
               inputId = 'profession_input',
               label = 'Whats your role as a Kenyan ? :',
               choices = unique(df2$VICTIMS)
             ),
             hr(),
             actionButton(
               inputId = 'button',
               label = 'Predict'
             ),
             hr(),
             h3('Here is the prediction for whether your county will be safe or not ...'),
             hr(),
             tableOutput(
               outputId = 'table_output'
             ),
             
             hr(),
             
             verbatimTextOutput(
               outputId = 'result_output'
             ),
             
             
             
             ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # First line graph
  output$year_kills_plot <- renderPlot({
   #  Years <- seq(min(year_kills$YEAR), max(year_kills$YEAR), length.out <- input$Years )
    plot(year_kills$YEAR, year_kills$V1, type = 'o', 
         # main = 'Time Series of number of killings against Year',
         xlab = 'Year', 
         ylab = 'Number of deaths',
         col = 'blue')
  })
  
  # Bar chart for victims
  
  output$county_plot <- renderPlot(
    {
      df_filtered <- filter(df2, df2$ADMIN1 == input$County_DD)
      ggplot(data = df_filtered, aes(x = factor(EVENT_TYPE)))+
        geom_bar(stat = 'count', fill = 'steelblue')
    }
  )
  
  # Barchart ya top 10 killings
  
  output$top10 <- renderPlot(
    {
      barplot(top10_counties$FATALITIES, names.arg = top10_counties$ADMIN1, col = 'red')
    }
  )
  
  output$bottom10 <- renderPlot(
    {
      barplot(bottom10_counties$FATALITIES, names.arg = bottom10_counties$ADMIN1, col = 'blue')
    }
  )
  
  # Prediction output
  
  df_shiny <- reactive({
    data.frame(
      ADMIN1 = input$county_input_DD,
      YEAR = input$year_input
    )
  })
  
  prediction <- reactive({predict(
    tree, df_shiny()
  )})
  
  final_result <- reactive(
    {
      if (prediction() > 1.5) {
        'The county specified will NOT be safe'
      } else {
        'The county will be safe from attacks'
      }
    }
  )
  
  observeEvent(input$button, {
    output$table_output <- renderTable(
      {
        df_shiny()
      }
    )
    
    output$result_output <- renderPrint(
      {
        final_result()
      }
    )
    
    
  })
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
