library(shiny)
library(ggplot2)
library(datasets)
library(googlesheets4)
library(here)
library(dplyr)

auth_cache <-
  if(interactive()){
  here("waterways", "secrets")
  } else {
    here("secrets")
  }

gs4_auth(cache = auth_cache)
secret_sheet <- "14qI8A51Op2Ri3yfwD1t2AQZ1fxki-KUFb7_EEyvO4Lo"

data <- NULL
data <- tryCatch(read_sheet(secret_sheet,),
                 error = identity)

ui <- fluidPage(
  titlePanel("Random questions"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      textOutput("test_text")
    )
  )
)

server <- function(input, output) {
  
  output$test_text <- renderText({
    
    if("error" %in% class(data)){
      text_to_show <- paste0("Error: ", error[["message"]])
    } else {
      text_to_show <- names(data)
    }
    text_to_show
  })
}

shinyApp(ui, server)
