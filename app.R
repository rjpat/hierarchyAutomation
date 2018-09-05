library(shiny)
library(dplyr)

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
      
      sliderInput('numTiers', 'Number of Tiers', 1, min = 1, max = 6, step = 1),

      
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
          tableOutput('sourceTable')
        ),
                          
        tabPanel('Output',
          tableOutput('finalTable')
                 )
      )
    )
  )
)
 #test

server <- function(input, output) {
  
  rawSourceData <- reactive({
    sourceData <- input$baseData
    
    if(is.null(sourceData))
      return(NULL)
    
    read.csv(sourceData$datapath, header = TRUE, fileEncoding="UTF-8-BOM")
  })
  
  tier1 <- reactive({
    if(input$columnName1 == 0)
      return(rawSourceData())
    
    name <- if(input$columnName1 != 0) 
      unique(select(rawSourceData(), input$columnName1))
    
    shortName <- if(input$columnShortName1 != 0) {
      unique(select(rawSourceData(), input$columnShortName1))
    } else {
      name
    }

    tier1Comb <- cbind(name, shortName)
    
    names(tier1Comb) <- c('Name', 'ShortName')
    
    data.frame(tier1Comb) %>%
      mutate(Parent = 0) %>%
      arrange(Parent, Name) %>%
      mutate(id = (row_number())) 
      
  })
  
  tier2 <- reactive({
    if(input$columnName2 == 0)
      return(tier1())
    
    #returns the column of this tiers name and the previous in order to get the appropriate parent
    name <- if(input$columnName2 != 0) 
      unique(select(rawSourceData(), c(input$columnName2, input$columnName1)))
    
    
    shortName <- if(input$columnShortName2 != 0) {
      unique(select(rawSourceData(), input$columnShortName2))
    } else {
      name[1]
    }
    
    
    
    tier2Comb <- cbind(name, shortName)
    
    names(tier2Comb) <- c('Name', 'ParentName', 'ShortNameInt')
    
    #Combine with tier1 to get the appropriate parent
    tier2Comb %>%
      inner_join(tier1(), by = c('ParentName' = 'Name')) %>%
      select(Name, ShortNameInt, id) %>%
      rename(ShortName = ShortNameInt, Parent = id) %>%
      arrange(Parent, Name) %>%
      mutate(id = (row_number() + max(tier1()$id)))
      
  })
  

  
  output$finalTable <- renderTable({
    combinedTiers <- if(input$numTiers == 1){
      tier1()
    } else if(input$numTiers == 2) {
      rbind(tier1(), tier2())
    }
    
    #Pre-allocate the appropriate columns for the below loop to fill
    combinedTiers <- combinedTiers %>%
      mutate(parentId = 0) %>%
      mutate(parentNum = 0) %>%
      mutate(futureId = 0)
    
    #set an out of bounds var as the starting point for one of the loops
    prevParent <- 10000

    for(i in combinedTiers$id) {
      currParent <- combinedTiers[i,3]

      #Determine if the previous parent is equivalent to this columns one
      if(currParent == prevParent) {
        parentNum = parentNum + 1
      } else {
        parentNum = 1
      }

      #add parentnum to $parentNum
      combinedTiers[i,6] <- parentNum

      #Update prevparent
      prevParent <- currParent

      if(combinedTiers[i,3] == 0) {
        parentId = 'a'
      } else {
        parentId = combinedTiers[combinedTiers[i,3],7]
      }

      combinedTiers[i,5] <- parentId

      combinedTiers[i,7] <- paste(parentId, parentNum, sep = ".")

    }
    
    #Select the non-backend columns which are required for data input to show in the final table
    combinedTiers %>%
      select(Name, ShortName, parentId, futureId)
  })
  
  output$sourceTable <- renderTable({
    rawSourceData()
    # tier3()
  })
}

shinyApp(ui = ui, server = server)