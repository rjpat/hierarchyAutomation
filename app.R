library(shiny)

columnNameInput <- function(inputNumber, inputType) {
  conditionalPanel(condition = paste0('input.numTiers > ', (inputNumber - 1)),
    numericInput(paste0('column', inputType, inputNumber), paste('Which Column is Tier', inputNumber, inputType, sep = ' '), 0, min = 1, max = 30, step = 1)
  )
}

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
      
      sliderInput('numTiers', 'Number of Tiers', 3, min = 1, max = 6, step = 1),

      
      
      
      column(6,
        columnNameInput(1, 'Name'),
        
        columnNameInput(2, 'Name'),
        
        columnNameInput(3, 'Name'),
        
        columnNameInput(4, 'Name'),
        
        columnNameInput(5, 'Name'),
        
        columnNameInput(6, 'Name')
      ),
      
      column(6,
        columnNameInput(1, 'ShortName'),
        
        columnNameInput(2, 'ShortName'),
        
        columnNameInput(3, 'ShortName'),
        
        columnNameInput(4, 'ShortName'),
        
        columnNameInput(5, 'ShortName'),
        
        columnNameInput(6, 'ShortName')
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
  
  rawSourceData <- reactive({
    sourceData <- input$baseData
    
    if(is.null(sourceData))
      return(NULL)
    
    read.csv(sourceData$datapath, header = TRUE, fileEncoding="UTF-8-BOM")
  })
  
  output$originalTable <- renderTable({
    if(input$columnName1 == 0)
      return(rawSourceData())
    
    rawSourceData()[,input$columnName1]
  })
}

shinyApp(ui = ui, server = server)