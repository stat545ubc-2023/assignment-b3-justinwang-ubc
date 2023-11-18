library(shiny)
library(tidyverse)
library(palmerpenguins)
penguins_data <- na.omit(penguins)  

ui <- fluidPage(
  ## include a css file to make it beautiful 
  includeCSS("www/styles.css"),
  
  titlePanel("Palmer Penguins Data"),
  
  ## feature 1: include a penguins data logo 
  img(src="logo.png"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("billLengthInput", "Bill Length (mm)", min(penguins_data$bill_length_mm), max(penguins_data$bill_length_mm), value = c(40, 50)),
      
      ## feature2: use uiOutput to show a UI input, because it allow me not to hard code all the type of species, also allow user to select mutiple species at the same time
      uiOutput("speciesSelector"),
      
      ## feature 3: give user a choice to whether sort the table 
      checkboxInput("sortBillLength", "Sort the table by bill length", FALSE)
    ),
    mainPanel(
      
      ## feature 4: allow user to view the table and his graph in different tab
      tabsetPanel(
        tabPanel("Histogram Graph",  
                 
                 ## feature5 : let user see how many result get by using textoutput
                 h2(textOutput("resultCount")),
                 plotOutput("coolplot"),
                 br(), br()
        ),
        tabPanel("Table", 
                 
                 ## feature6: allow user to download the filtered table
                 downloadButton("downloadData", "Download Data"),
                 
                 #feature 7: use DT package to beautify the table 
                 DT::dataTableOutput("results")
        )
      )
    )
  )
)
server <- function(input, output) {
  
  ## filter the data set based on the input and store the result into a variable 
  filtered <- reactive({
    if(is.null(input$speciesInput)){
      return (NULL)
    }
    penguins_data %>%
      filter(bill_length_mm >= input$billLengthInput[1],
             bill_length_mm <= input$billLengthInput[2],
             species %in% input$speciesInput)
  })
  
  ## feature 2 logic to render ui that allow user select multiple species 
  output$speciesSelector <- renderUI({
    checkboxGroupInput("speciesInput", "Species", choices = sort(unique(penguins_data$species)), selected = unique(penguins_data$species)[1])
  })
  
  output$resultCount <- renderText({
    paste("Number of penguins found:", nrow(filtered()))
  })
  
  ## logic to plot the his graph based on the filtered data 
  output$coolplot <- renderPlot({
    if(is.null(filtered())){
      return()
    }
    ggplot(filtered(), aes(bill_length_mm)) +
      geom_histogram(binwidth = 1)
  })
  ##  feature 7 logic 
  output$results <- DT::renderDataTable({
    data <- filtered()
    ## feature 3 logic 
    if(input$sortBillLength){
      data <- data %>% arrange(bill_length_mm)
        
    }
    data
  })
  ## download logic 
  output$downloadData <- downloadHandler(
    filename ="penguin_data.csv",
    content = function(file){
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
  
  
}


shinyApp(ui = ui, server = server)