library(shiny)
library(ggplot2)
library(googlesheets4)
library(here)
library(dplyr)
library(stringr)
library(carutools)
library(janitor)
library(lubridate)

#### THIS CODE ALWAYS RUNS #####################################################

# credentials and authentication -----------------------------------------------
gs4_deauth()
auth_cache <- if_else(interactive(), "app/secrets", "secrets")
gs4_auth(email = TRUE, cache = auth_cache)

# attempt to read sheet --------------------------------------------------------
secret_sheet <- "14qI8A51Op2Ri3yfwD1t2AQZ1fxki-KUFb7_EEyvO4Lo"
data <- tryCatch(read_sheet(secret_sheet), error = identity)

if(is.data.frame(data)){

  ## Clean names and rename ----------------------------------------------------

  data <- clean_names(data)

  data <- rename(data,
                 month = in_what_month_of_the_year_did_these_conversations_take_place,
                 n_meaningful = how_many_meaningful_conversations_have_you_had_within_the_reporting_period,
                 n_general    = how_many_general_conversations_have_you_had_within_the_reporting_period,
                 people       = how_would_you_describe_the_people_you_have_spoken_to_please_tick_all_that_apply,
                 concerns     = which_of_the_following_concerns_were_identified_by_your_conversations,
                 comments     = do_you_have_any_other_comments_about_your_recent_interactions_that_you_would_like_to_share)

  ## Tally counts from comma-delimited string columns (widening data) ----------

  data <- mutate(data, across(c(people, concerns), str_to_lower))

  data <- tally_delimited_string(data, people)
  data <- tally_delimited_string(data, concerns)
}

all_months <-
  c("January", "February", "March", "April", "May",
    "June", "July", "August", "September", "October",
    "November", "December")

#### USER INTERFACE ############################################################

ui <- fluidPage(
  titlePanel("Random questions"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("months", "Months",
                         choices  = all_months,
                         selected = all_months)
    ),
    mainPanel(
      textOutput("test_text"),
      tableOutput("test_table")
    )
  )
)

#### SERVER ####################################################################

server <- function(input, output) {

  # test_text ------------------------------------------------------------------
  output$test_text <- renderText({

    if("error" %in% class(data)){

      text_to_show <- paste0("Error: ", error[["message"]])

      } else {

        text_to_show <-
          str_c("Succesfully reading a Google sheet with these column names:",
                str_c(names(data), collapse = " "),
                collapse = "\n\n")
      }

    text_to_show

    }) # /test_text

  # test_table ----------------------------------------------------------------=
  output$test_table <- renderTable({
    data |>
      filter(month %in% input$months)
  })
}

#### RUN APP ###################################################################

shinyApp(ui, server)
