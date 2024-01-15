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
library(scales)
library(RColorBrewer)
library(snakecase)
library(forcats)


#### THIS CODE ALWAYS RUNS #####################################################

# credentials and authentication -----------------------------------------------
gs4_deauth()

is_app_dir <- file_exists(here(".appDir"))
auth_cache <- "secrets"
gs4_auth(email = TRUE, cache = auth_cache)

## Miscellaneous helpers -------------------------------------------------------

capitalise <- function(x){
  first <- str_to_upper(str_sub(x, 1, 1))
  rest  <- str_sub(x, 2, -1)
  str_c(first, rest)
}

today <- function(){
  Sys.Date()
}

year_ago <- function(from = today()){
  from - days(365)
}

# attempt to read sheet --------------------------------------------------------
secret_sheet <- "14qI8A51Op2Ri3yfwD1t2AQZ1fxki-KUFb7_EEyvO4Lo"
data <- tryCatch(read_sheet(secret_sheet), error = identity)

if(is.data.frame(data)){

  data <- clean_names(data)

  data <- rename(data,
                 month = in_what_month_of_the_year_did_these_conversations_take_place,
                 n_meaningful = how_many_meaningful_conversations_have_you_had_within_the_reporting_period,
                 n_general    = how_many_general_conversations_have_you_had_within_the_reporting_period,
                 people       = how_would_you_describe_the_people_you_have_spoken_to_please_tick_all_that_apply,
                 concerns     = which_of_the_following_concerns_were_identified_by_your_conversations,
                 comments     = do_you_have_any_other_comments_about_your_recent_interactions_that_you_would_like_to_share)

  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
  ##############################################################################
  ##    DELET THIS // DELET THIS // DELET THIS // DELET THIS // DELET THIS     #
  data <- filter(data, month != "January")
  ##############################################################################
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
  ## Clean names and rename ----------------------------------------------------


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
                values_from = is_concern,
                names_prefix = "concern_")

  generalised_data <-
    data |>
    select(-starts_with("concern_")) |>
    left_join(concerns, by = "response_id")

  generalised_concerns <-
    select(generalised_data, starts_with("concern_")) |>
    names()

  generalised_concerns <-
    str_remove(generalised_concerns, "concern_") |>
    str_replace_all("_", " ") |>
    str_remove_all("[:punct:]") |>
    str_squish() |>
    capitalise() |>
    sort()
}




#### USER INTERFACE ############################################################

p_size <- function(..., size = 1, em = TRUE) p(..., style = str_c("font-size:", size, if_else(em, "em", "px"), ";"))

ui <- fluidPage(
  tabsetPanel(

    ## AT A GLANCE =============================================================
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
             fluidRow(
               h2("These are some of the concerns that have been raised:"),
               p(""),
               sidebarLayout(
                 sidebarPanel(

                   dateRangeInput(
                     "concerns_plot_daterange",
                     label = "Show concerns raised between:",
                     start = year_ago(),
                     end = today(),
                     min = "2022-01-01",
                     max = today()),

                   checkboxGroupInput(
                     "concerns_plot_highlight",
                     "Concerns to highlight:",
                     choices = generalised_concerns,
                     selected = sample(generalised_concerns, 2)
                     ),

                   width = 2
                   ),
               mainPanel(
                 plotOutput("concerns_plot", width = "90%", height = "600px")
                 ))))),

    tabPanel("Who are we talking to?"),


    ## WHAT ARE WE TALKING ABOUT? ==============================================
    tabPanel("What are we talking about?",
             plotOutput("horizontal_concerns_bar", height = "600px")),

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

  output$concerns_plot <- renderPlot({

    plot_data <-
      generalised_data |>
      pivot_longer(starts_with("concern_"),
                   names_prefix = "concern_",
                   names_to = "concern",
                   values_to = "identified") |>

      # NB currently we treat 1 or more occurrences of the same concerns within
      # a month by the same chaplain as equivalent values, since a return could
      # be daily or monthly. It's hard to get Google forms to allow people to tally
      # occurrences ongoingly, so currently this seems like the most responsible way
      # to report
      summarise(occured = any(identified), .by = c(concern, email_address, month)) |>
      summarise(count = sum(occured), .by = c(concern, month)) |>
      mutate(concern =
               capitalise(concern) |>
               str_replace_all("_", " ") |>
               ordered() |>
               fct_reorder(-count))

    highlight <- filter(plot_data, concern %in% input$concerns_plot_highlight)
    lowlight  <- filter(plot_data, !concern %in% input$concerns_plot_highlight)

    ggplot(lowlight, aes(x = month, y = count, group = concern)) +

      geom_line(colour = "gray60", alpha = 0.35) +

      geom_line(data = highlight, aes(x = month, y = count, colour = concern),
                size = 2, alpha = 0.85) +

      scale_colour_discrete() +

      scale_x_date(
        breaks = "1 month",
        limits = c(input$concerns_plot_daterange[1],
                   input$concerns_plot_daterange[2]),
        date_labels = "%b %y",
        guide = guide_axis(n.dodge = 2)) +

      theme_ca() +
      theme(
        text = element_text(size = 28),
        panel.grid.minor = element_blank()
      ) +

      ylab("Chaplains who encountered this issue") +
      xlab("Month")
  })
  ##/ outputs for page 1 /


  ## Download handler ----------------------------------------------------------
  output$xlsx_download <- downloadHandler(
    filename = str_c("waterways-chaplains-interactions_", Sys.Date(), ".csv"),
    content = function(file){
      vroom_write(data, file, delim = ",")
    }
  )

  ## Horizontal concerns plot --------------------------------------------------
  output$horizontal_concerns_bar <- renderPlot({

    very_concise_concerns <-
      data |>

      pivot_longer(starts_with("concerns_"),
                   names_to = "concern",
                   values_to = "indicated") |>

      summarise(count = sum(indicated),
              .by = c(month, concern))


    ggplot(very_concise_concerns, aes(x = count, y = reorder(concern, count), fill = concern)) +

      geom_col() +

      scale_fill_manual(values = rep(brewer.pal(12, "Set3"),
                                     length.out = nrow(very_concise_concerns))
                        ) +
      scale_y_discrete(
        labels = \(x){
          str_remove(x, "concerns_") |>
            str_replace("un_employment", "unemployment") |>
            to_title_case() |>
            str_replace("Ptsd", "PTSD")
        }
      ) +

      labs(x = "Count",
           y = "Conversation topics",
           title = "What are we talking about?") +

      theme_ca("black") +
      theme(legend.position = "none",
            text = element_text(size = 28),
            panel.grid.major.y = element_blank())

  })

  ## Pie chart people plot -----------------------------------------------------

  ggplot(very_concise_people, aes(x = people, y = count, fill = people)) +

    geom_col(width = 1) +

    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +

    theme_minimal() +

    scale_fill_manual(values = rep(brewer.pal(12, "Set3"), length.out = nrow(very_concise_people))) +

    theme_ca("black") +

    labs(x = "", y = "",
         title = "Who are we talking to?") +

    theme(legend.position = "none") +

    scale_x_discrete(labels = ~ str_remove(.x, "people")  |>
                       str_replace("wo_men", "women") |>
                       to_title_case()) +

    coord_polar()


}

#### RUN APP ###################################################################

shinyApp(ui, server)
