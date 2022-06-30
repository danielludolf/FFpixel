
library(shiny)
library(tidyverse)

# proportion pixel size data set
dat <- readr::read_csv('pixel_size.csv', show_col_types = FALSE)

# function that outputs pixel dimensions for relative risk/risk ratios and proportions
pixel_size_output <- function(type, number){
  
  if(type == "Relative Risk"){
    
    # range for relative risk is assumed 0-6 at the moment
    rr_pixel <- number * (95/5) + 5 - 19
    
    return(tibble(`Pixel Dimension` = paste0(as.character(sprintf("%01.2f", rr_pixel))," x ", 
                                             as.character(sprintf("%01.2f", rr_pixel)))))
    
  }
  
  else if(type == "Proportion (Normal)"){
    
    if(number > 1){
      stop("Please enter proportions in decimal format")
    }
    
    pos_int <- round(number,2)
    
    pos_val <- dat %>% 
      filter(Positive == pos_int) %>% 
      mutate(Value = paste0(sprintf("%01.2f", Value)," x ", sprintf("%01.2f", Value))) %>% 
      select(`Pixel Dimension` = Value)
    
    return(pos_val)
    
  }
  
  else{
    
    if(number > 1){
      stop("Please enter proportions in decimal format")
    }
    
    neg_int <- round(number,2)
    
    neg_val <- dat %>% 
      filter(Negative == neg_int) %>% 
      mutate(Value = paste0(sprintf("%01.2f", Value)," x ", sprintf("%01.2f", Value))) %>% 
      select(`Pixel Dimension` = Value)
    
    return(neg_val)
    
  }
}

ui <- fluidPage(

    titlePanel("Pixel Size for Proportions and Relative Risk"),

    # Create Radio Button Options and numeric input
    ui <- fluidPage(
      radioButtons("metric", "Is the Pixel Size for a Proportion or Relative Risk?", 
                   c("Proportion (Normal)", "Proportion (Opposite)", "Relative Risk"), inline = T),
      numericInput("num", "Value", value = 0, min = 0, max = 1)
    ),

    # output in console text format
    mainPanel(
       verbatimTextOutput("code")
    )
)

server <- function(input, output) {
  
    output$code <- renderPrint({
      
      # print the pixel dimension based on radio button option and inputted value
      pixel_size_output(input$metric, input$num)
      
    })
}

shinyApp(ui = ui, server = server)
