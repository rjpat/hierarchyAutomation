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
    
    read.csv(sourceData$datapath, header = TRUE, fileEncoding="UTF-8-BOM", na.strings = c("", "NA"))
  })
  

  
  final <- reactive({
    loadFrame <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('name', 'shortName', 'parent', 'futureId'))
    
    data <- rawSourceData() %>%
      mutate(parent0 = 'a')
    
    tierLoops <- function(tierName, tierShort, tierNum) {
      
      #'Intermediate produced here to remove any issues of shortname not being assigned. 
      #'If there is no designated shortname then the name is duplicated in the main table and assigned as short.
      #'The reason for duplication is to prevent a select issue where you cannot select the same column multiple times
      tierShortInt <- if(tierShort == 0) {
        data <- data %>%
          bind_cols(data[tierName])
        
        tierShort <- ncol(data)
      } else {
        tierShort
      }
      
      prevDataParent <- paste0('parent', tierNum-1)
      
      cleansed <- data %>%
        select(tierName, tierShort, prevDataParent) %>%
        unique() %>%
        rename('name' = !!names(.[1]), 
               'shortName' = !!names(.[2]), 
               'parent' = !!names(.[3])) %>%
        #Removes any rows with an na name or parent (where previous tier had an na name)
        filter(!is.na(name)) %>%
        filter(!is.na(parent)) %>%
        #Arranges the data in the correct order then adds the additional background columns required for producing the hierarchy
        arrange(parent, name, shortName) %>%
        mutate(shortName = trimws(substring(shortName, 1, 25)), 
               id = row_number(), 
               parentNum = 0, 
               futureId = 0) 
      
      prevParent <- 0
      
      #Produces the area hierarchy details through the loop
      for(i in cleansed$id) {
        currParent <- cleansed[i, 3]
        
        if(currParent == prevParent) {
          parentNum = parentNum + 1
        } else {
          parentNum = 1 
        }
        
        cleansed[i, 5] <- parentNum
        
        cleansed[i, 6] <- paste0(currParent, '.', parentNum)
        
        prevParent = currParent
      }
      
      #removes non-necessary columns
      cleansedPrep <<- cleansed %>%
        select(name, shortName, parent, futureId)
      
      #Creates the loading dataframe to be utilised at the end
      loadFrame <<- rbind(loadFrame, cleansedPrep)
      
      #Prep and join the data back to the original dataset allowing for the function to be called again
      cleansedPrep2 <- cleansedPrep %>%
        select(name, parent, futureId) 
      
      named <- colnames(data[tierName])
      named2 <- colnames(data[prevDataParent])
      
      data <<- data %>%
        left_join(cleansedPrep2, by = c(setNames('name', named), setNames('parent', named2))) %>%
        rename(!!paste0('parent', tierNum) := futureId)
    
      return(loadFrame)
    }
    
    for(i in 1:(max(input$numTiers))) {
      a <- eval(parse(text = (paste0('input$column', 'Name', i))))

      b <- eval(parse(text = (paste0('input$column', 'ShortName', i))))

      tierLoops(a, b, i)
    }
    
    loadFrame
  })

  
  output$sourceTable <- renderTable({
    rawSourceData()
    # tier3()
  })
  
  output$finalTable <- renderTable({
    final()
  })
}

shinyApp(ui = ui, server = server)