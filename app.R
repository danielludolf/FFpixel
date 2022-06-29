
library(shiny)
library(tidyverse)

dat <- readr::read_csv('~/FFpixel/pixel_size.csv', show_col_types = FALSE)

pixel_size_output <- function(type, number){
  
  if(type == "Relative Risk"){
    
    rr_pixel <- number * (95/5) + 5 - 19
    
    return(tibble(`Pixel Dimension` = rr_pixel))
    
  }
  
  else if(type == "Proportion (Positive)"){
    
    if(number > 1){
      stop("Please enter proportions in decimal format")
    }
    
    pos_int <- round(number,2)
    
    pos_val <- dat %>% 
      filter(Positive == pos_int) %>% 
      mutate(Value = sprintf("%01.2f", Value)) %>% 
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
      mutate(Value = sprintf("%01.2f", Value)) %>% 
      select(`Pixel Dimension` = Value)
    
    return(neg_val)
    
  }
}

ui <- fluidPage(

    titlePanel("Pixel Size for Proportions and Relative Risk Ratios"),

    ui <- fluidPage(
      radioButtons("metric", "Is the Pixel Size for a Proportion or Relative Risk?", 
                   c("Proportion (Positive)", "Proportion (Negative)", "Relative Risk"), inline = T),
      numericInput("num", "Value", value = 0, min = 0, max = 1)
    ),

    mainPanel(
       verbatimTextOutput("code")
    )
)

server <- function(input, output) {
  
    output$code <- renderPrint({
      
      pixel_size_output(input$metric, input$num)
      
    })
}

shinyApp(ui = ui, server = server)
