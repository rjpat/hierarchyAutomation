library(shiny)

ui <- fluidPage(#theme = 'mintyTheme.css',
  titlePanel('Area Hierarchy Creation'),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        'baseData',
        'Input Area Hierarchy .csv',
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      
      numericInput(
        'numTiers',
        'Number of Tiers',
        5,
        min = 1,
        max = 6,
        step = 1
      ),
      column(6,
        numericInput(
          'columnName1',
          'Which Column is Tier 1 Name',
          1,
          min = 1,
          max = 30, 
          step = 1
        ),
        
        numericInput(
          'columnName2',
          'Which Column is Tier 2 Name',
          1,
          min = 1,
          max = 30, 
          step = 1
        ),
        
        numericInput(
          'columnName3',
          'Which Column is Tier 3 Name',
          1,
          min = 1,
          max = 30, 
          step = 1
        ),
        
        numericInput(
          'columnName4',
          'Which Column is Tier 4 Name',
          1,
          min = 1,
          max = 30, 
          step = 1
        ),
        
        numericInput(
          'columnName5',
          'Which Column is Tier 5 Name',
          1,
          min = 1,
          max = 30, 
          step = 1
        ),
        
        numericInput(
          'columnName6',
          'Which Column is Tier 6 Name',
          1,
          min = 1,
          max = 30, 
          step = 1
        )
      ),
      column(6,
         numericInput(
           'columnShortName1',
           'Which Column is Tier 1 Short Name',
           1,
           min = 1,
           max = 30, 
           step = 1
         ),
         
         numericInput(
           'columnShortName2',
           'Which Column is Tier 2 Short Name',
           1,
           min = 1,
           max = 30, 
           step = 1
         ),
         
         numericInput(
           'columnShortName3',
           'Which Column is Tier 3 Short Name',
           1,
           min = 1,
           max = 30, 
           step = 1
         ),
         
         numericInput(
           'columnShortName4',
           'Which Column is Tier 4 Short Name',
           1,
           min = 1,
           max = 30, 
           step = 1
         ),
         
         numericInput(
           'columnShortName5',
           'Which Column is Tier 5 Short Name',
           1,
           min = 1,
           max = 30, 
           step = 1
         ),
         
         numericInput(
           'columnShortName6',
           'Which Column is Tier 6 Short Name',
           1,
           min = 1,
           max = 30, 
           step = 1
         )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Input',
          tableOutput('originalTable')
        ),
                          
        tabPanel('Output')
      )
    )
  )
)


server <- function(input, output) {
  
  rawData <- reactive({
    infile <- input$baseData
    
    if(is.null(infile))
      return(NULL)
    
    read.csv(infile$datapath, header = TRUE, fileEncoding="UTF-8-BOM")
  })
  
  output$originalTable <- renderTable({
    rawData()
  })
}

shinyApp(ui = ui, server = server)