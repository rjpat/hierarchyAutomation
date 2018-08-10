library(shiny)

ui <- fluidPage(#theme = 'mintyTheme.css',
   titlePanel('Area Hierarchy Creation'),
   
   sidebarLayout(
     sidebarPanel(
       fileInput('baseData', 'Input Area Hierarchy .csv',
                 multiple = FALSE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
       
       numericInput('numTiers', 'Number of Tiers', 5, min = 1, max = 6, step = 1)
     
      
      ),
     
     mainPanel(
       tabsetPanel(
         tabPanel('Input'),
         
         tabPanel('Output')
       )
     )
   )
)


server <- function(input, output) {
   

  
}

shinyApp(ui = ui, server = server)