library(shiny)
library(bslib)
library(tidyverse)
library(tableone)




# Define UI for data upload app ----
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "lux",
    #base_font = font_google("Inter"),
    #navbar_bg = "#25443B"
  ), 
  
  # App title ----
  titlePanel("SMD calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Header ----
      checkboxInput("header", "Header", TRUE),
      
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Input: grouping variable
      textInput("grv", "Variable to group by"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Show",
                   choices = c(Data = "data",
                               Table = "table"),
                   selected = "data")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        mydata <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote) %>% 
          as.data.frame()
        
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "data") {
      return(mydata)
      
    }
    else {
      
      colnames(mydata)[which(colnames(mydata)==input$grv)] <- "group_var"
      covars <- mydata %>% select(-group_var) %>% colnames
      res <- CreateTableOne(vars = covars, strata = "group_var", data = mydata, smd = TRUE)
      res2 <- res %>% ExtractSmd() %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(Variables = rowname)
      
      return(res2)
     
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)