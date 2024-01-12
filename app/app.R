library(shiny)
library(ggplot2)
library(googlesheets4)
library(here)
library(dplyr)
library(stringr)
library(carutools)
library(janitor)
library(lubridate)
library(fs)
library(vroom)
library(tidyr)

#### THIS CODE ALWAYS RUNS #####################################################

# credentials and authentication -----------------------------------------------
gs4_deauth()

is_app_dir <- file_exists(here(".appDir"))
auth_cache <- "secrets"
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

  data <- mutate(data, response_id = str_c("r_", row_number()))

  ## Converting conversations to numeric ---------------------------------------

  conversation_responses <- c("None", "One", "Two", "Three", "Four", "Five or more")

  data <- mutate(
    data,
    across(c(n_meaningful, n_general),
           \(x){
             match(x, conversation_responses) - 1
           }
           )
    )

  ## Add month-level date-time marker

  data <-
    mutate(data,
           month =
             str_c(year(timestamp), match(month, month.name), "01",
                   sep = "-") |>
             ymd())

  ## Adding concern group codes to data ----------------------------------------

  concerns_categories <- vroom("data/concerns-categories.csv", delim = ",", col_types = "cc")

  concerns <-

    pivot_longer(data, starts_with("concerns_"),
                 names_to = "concern", values_to = "is_concern") |>

    select(response_id, concern, is_concern) |>

    left_join(concerns_categories, by = c("concern")) |>

    summarise(is_concern = any(is_concern),
              .by = c(response_id, concern_category)) |>

    pivot_wider(names_from = concern_category,
                values_from = is_concern)

  generalised_data <-
    data |>
    select(-starts_with("concern_")) |>
    left_join(concerns, by = "response_id")
}


#### USER INTERFACE ############################################################

p_size <- function(..., size = 1, em = TRUE) p(..., style = str_c("font-size:", size, if_else(em, "em", "px"), ";"))

ui <- fluidPage(
  tabsetPanel(
    tabPanel("At a glance",
           fluidPage(
             fluidRow(
               div(
                 h1("So far this year:"),
                 p("We have had over ",
                   strong(textOutput("total_general", inline = TRUE)), " conversations and over",
                   strong(textOutput("total_meaningful", inline = TRUE)), " meaningful conversations on Britain's Waterways."),
                 style = "font-size:2em;"
             )
           ),
             fluidRow()
           )),
    tabPanel("Who are we talking to?"),
    tabPanel("What are we talking about?"),
    tabPanel(
      "Download data",
      downloadButton("xlsx_download", "Download data")
    ),
    tabPanel("Detailed graphs and tables")
  )
)

#### SERVER ####################################################################

server <- function(input, output) {

  ## outputs for page 1: -------------------------------------------------------

  output$total_meaningful <- renderText({
    as.character(sum(data[["n_meaningful"]], na.rm = TRUE))
  })

  output$total_general <- renderText({
    as.character(sum(data[["n_general"]], na.rm = TRUE))
  })
  ##/ outputs for page 1 /

}

#### RUN APP ###################################################################

shinyApp(ui, server)
