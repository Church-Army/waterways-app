library(shiny)
library(ggplot2)
library(googlesheets4)
library(googledrive)
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
library(digest)
library(shinyWidgets)
library(purrr)


#### THIS CODE ALWAYS RUNS #####################################################

# credentials and authentication -----------------------------------------------
gs4_deauth()
drive_deauth()

auth_cache <- "secrets"
gs4_auth(email = TRUE, cache = auth_cache)
drive_auth(email = TRUE, cache = auth_cache)

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

prettify <- function(x) {
  capitalise(x) |>
    str_replace_all("_", " ")
}

fill_colours <-
  c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
    "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
    "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
    "#8A7C64", "#599861")

# attempt to read sheet --------------------------------------------------------
secret_sheet <- "14qI8A51Op2Ri3yfwD1t2AQZ1fxki-KUFb7_EEyvO4Lo"
data <- tryCatch(read_sheet(secret_sheet), error = identity)

if(is.data.frame(data)){

  data <- clean_names(data)

  data <- rename(data,
                 month        = in_what_month_of_the_year_did_these_conversations_take_place,
                 hub          = which_hub_are_you_reporting_from,
                 n_meaningful = how_many_meaningful_conversations_have_you_had_within_the_reporting_period,
                 n_general    = how_many_general_conversations_have_you_had_within_the_reporting_period,
                 people       = how_would_you_describe_the_people_you_have_spoken_to_please_tick_all_that_apply,
                 concerns     = which_of_the_following_concerns_were_identified_by_your_conversations,
                 comments     = do_you_have_any_other_comments_about_your_recent_interactions_that_you_would_like_to_share)

  data <-
    relocate(data, hub, .after = month) |>
    mutate(hub  = factor(hub))

  ## Tally counts from comma-delimited string columns (widening data) ----------

  data <- mutate(data, across(c(people, concerns), str_to_lower))

  data <- tally_delimited_string(data, people)
  data <- tally_delimited_string(data, concerns)

  data <- mutate(data, response_id = str_c("r_", row_number()))

  ## Converting conversations to numeric ---------------------------------------

  data <- mutate(data,
                 across(where(is.list),
                        \(x){
                          modify_if(x, is.null, \(y) NA) |>
                            as.character()
                        })
  )

  conversation_responses <- c("None", "One", "Two", "Three", "Four", "Five or more")

  data <- mutate(
    data,
    across(c(n_meaningful, n_general),
           \(x){
             numeric_x <- as.numeric(x)
             matched_x <- match(x, conversation_responses) - 1

             matched_x[is.na(matched_x)] <- numeric_x[is.na(matched_x)]
             matched_x[is.na(matched_x)] <- 0

             matched_x
           }
           )
    )

  ## Add month-level date-time marker

  data <-
    mutate(data,
           month =
             str_c(year(timestamp), match(month, month.name), "01",
                   sep = "-") |>
             ymd(),
           # if this month hadn't started at the time of data collection,
           # assume we're talking about the nearest preceding month
           # (A 'December' collection in Jan will be for December last year.)
           month = if_else(month > timestamp, month - period("year"), month)
           )

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
}

#### USER INTERFACE ############################################################

concerns_picker <- function(...,
                            prefix,
                            choices = NULL,
                            randomly_select = length(choices),
                            hubs = NULL){
  sidebarPanel(
    ...,
    dateRangeInput(
      str_c(prefix, "_daterange"),
      label = "Show concerns raised between:",
      start = year_ago(),
      end = today(),
      min = "2022-01-01",
      max = today()),

    if(!is.null(hubs)){
      selectInput(
        str_c(prefix, "_hubs"),
        "Hub:",
        choices = c("All", sort(hubs)),
        selected = "All"
      )},

    checkboxGroupInput(
      str_c(prefix, "_highlight"),
      "Concerns to highlight:",
      choices = choices,
      selected = sample(choices, randomly_select)
      )
    )
}

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
                 uiOutput("concerns_picker_mainpage"),
               mainPanel(
                 plotOutput("concerns_plot", width = "90%", height = "600px"),
                 width = 10
                 ))))),

    tabPanel("Who are we talking to?",
             plotOutput("people_pie_chart")
             ),


    ## WHAT ARE WE TALKING ABOUT? ==============================================
    tabPanel("What are we talking about?",
               uiOutput("concerns_picker_hbar"),
               mainPanel(plotOutput("horizontal_concerns_bar", height = "600px"))
             ),

    tabPanel("Stories and other comments",
             br(),br(),
             p("Stories and other comments collected through the reporting form are reviewed by hub leaders.\nThey cannot be viewed here because they may contain sensitive or confidential information.")
    ),

    tabPanel(
      "Admin area",
      passwordInput("admin_password", label = NULL, placeholder = "password"),
      actionButton("password_entry", "Enter password"),
      textOutput("password_msg"),
      uiOutput("admin_area")
      )
    )
  )

#### SERVER ####################################################################

server <- function(input, output) {

  ## Password protection for admin area === === === === === === === === === ===
  valid_password <- reactiveVal(FALSE)

  observeEvent(input$password_entry,{

    password <- readLines("secrets/admin_password")

    hash <- sha1(input$admin_password)

    if(hash == password){
      valid_password(TRUE)
      output$password_msg <- renderText("Password accepted")
    } else{
      valid_password(FALSE)
      output$password_msg <- renderText("Incorrect password")
      }
})

  ## Admin area UI === === === === === === === === === === === === === === ===
  output$admin_area <- renderUI({
    if(valid_password()){
      fluidPage(
        downloadButton("xlsx_download", "Download data"),
        h2("Generate monthly reports"),
        fluidRow(
          checkboxGroupInput("report_months", "Month(s)",
                             choices = month.name,
                             selected = month.name[month(today())],
                             inline = TRUE),
          radioButtons("report_year", "Year",
                       choices = -1:1 + year(today()),
                       selected = year(today()),
                       inline = TRUE),
          selectInput("report_type", "What kind of report should be generated?",
                      choices = c("Comprehensive reports", "'Other comments' reports"),
                      selected =  "Comprehensive reports"),
          p("Comprehensive reports include summary data for each respondent for each month specified. 'Other comments' reports show all 'other comments' responses for the selected timeframe."),
          checkboxInput("separate_reports", "Generate a separate report for each month",
                        value = TRUE)
        ),
        actionButton("generate_reports", "Generate monthly reports"),
        verbatimTextOutput("report_gen_message")
      )
    } else {
      renderText("Please enter a valid password to access this area of the site")
    }

  })

  ## Generate reports on button press ------------------------------------------
  observeEvent(input$generate_reports, {

    showModal(
      modalDialog("Generating reports...",
                  easyClose = TRUE)
      )

    months <- match(input$report_months, month.name)
    year <- input$report_year

    report_data <-
      data |>
      filter(month(month) %in% months, year(month) == year) |>
      group_by(month)


    if(isolate(input$report_type) == "Comprehensive reports"){

      drive_dir <- "Monthly summary reports"
      file_prefix <- "monthly-summary"

      conversations <-
        group_by(report_data, month, email_address) |>
        summarise(
          n_meaningful = sum(n_meaningful),
          n_general = sum(n_general)
        )

      counts <-
        group_by(report_data, month, email_address) |>
        summarise(across(starts_with(c("people_", "concerns_")), sum))

      report_data <- left_join(conversations, counts, by = c("email_address", "month"))

    } else if(isolate(input$report_type) == "'Other comments' reports"){

      drive_dir <- "'Other comments' reports"
      file_prefix <- "comments"

      report_data <-
        select(report_data, email_address, month, comments) |>
        filter(!is.na(comments))

    }

    rm(conversations, counts)

    ## generate reports
    if(input$separate_reports){
    report_data <- group_by(report_data, month)
    report_months <- group_keys(report_data)[["month"]]

    report_data <-
      group_split(report_data) |>
      set_names(nm = report_months)
    } else {
      month_range_label <-
        str_c(
          min(report_data[["month"]]), "to", max(report_data[["month"]]),
          sep = "-"
          )

      report_data <- list(report_data)
      names(report_data) <- month_range_label
    }

    tmp <- dir_create("tmp")

    iwalk(report_data,
          \(data, month){

            file_name <- str_c(file_prefix, month, sep = "_")

            report_sheet <- gs4_create(file_name, sheets = data)

            drive_path <- path("waterways chaplains reporting",
                               drive_dir, file_name)

            drive_mv(report_sheet, path = drive_path, overwrite = TRUE)

          })

    removeModal()

    output$report_gen_message <- renderPrint({
      reports <- length(report_data)
      cat("Generated", reports, "report(s) with the following name(s):\n")
      cat(str_c(file_prefix, names(report_data), sep = "_"), sep = "\n")
      cat("\nThese reports can be found in the Google Drive folder 'waterways chaplains reporting/",
          drive_dir, "'.",
          sep = "")
    })

    dir_delete("tmp")
  })

  ## outputs for page 1: -------------------------------------------------------
  output$total_meaningful <- renderText({
    as.character(sum(data[["n_meaningful"]], na.rm = TRUE))
  })

  output$total_general <- renderText({
    as.character(sum(data[["n_general"]], na.rm = TRUE))
  })


  output$horizontal_concerns_bar <- renderPlot({ggplot()})
  mainpage_plot_data <-
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
             prettify(concern) |>
             ordered() |>
             fct_reorder(-count))



  output$concerns_picker_mainpage <- renderUI({
    concerns_picker(prefix = "concerns_mainpage",
                    choices = unique(mainpage_plot_data[["concern"]]),
                    randomly_select = 2,
                    width = 2)
  })

  output$concerns_plot <- renderPlot({

    highlight <- filter(mainpage_plot_data, concern %in% input$concerns_mainpage_highlight)
    lowlight  <- filter(mainpage_plot_data, !concern %in% input$concerns_mainpage_highlight)

    ggplot(lowlight, aes(x = month, y = count, group = concern)) +

      geom_line(colour = "gray60", alpha = 0.35) +

      geom_line(data = highlight, aes(x = month, y = count, colour = concern),
                linewidth = 2, alpha = 0.85) +

      scale_colour_discrete() +

      scale_x_date(
        breaks = "1 month",
        limits = c(input$concerns_mainpage_daterange[1],
                   input$concerns_mainpage_daterange[2]),
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
      download_content <- select(data, -email_address)
      vroom_write(download_content, file, delim = ",")
    }
  )


  very_concise_concerns <-
    data |>
    pivot_longer(starts_with("concerns_"),
                 names_to = "concern",
                 values_to = "indicated") |>
    mutate(concern =
             str_remove(concern, "concerns_") |>
             capitalise() |>
             str_replace_all("_", " ") |>
             ordered())

  output$concerns_picker_hbar <- renderUI({
    concerns_picker(prefix = "concerns_hbar",
                    choices = unique(very_concise_concerns[["concern"]]),
                    width = 2,
                    hubs = unique(as.character(very_concise_concerns[["hub"]])),
                    switchInput("is_concerns_pie",
                                label = "Switch to:",
                                onLabel = "Bar chart",
                                offLabel = "Pie chart"))
    })
  ## Horizontal concerns plot --------------------------------------------------
  output$horizontal_concerns_bar <- renderPlot({
    ## apply sidebar preferences

    if(!is.null(input$concerns_hbar_hubs) && !input$concerns_hbar_hubs == "All"){
      very_concise_concerns <-
        filter(very_concise_concerns, hub %in% input$concerns_hbar_hubs) |>
        summarise(count = sum(indicated),
                  .by = c(month, concern, hub))
    } else {
      very_concise_concerns <-
        summarise(very_concise_concerns,
                  count = sum(indicated),
                  .by = c(month, concern))
    }

    if(!is.null(input$concerns_hbar_daterange[1])){
    very_concise_concerns <-
      filter(very_concise_concerns,
             between(month, input$concerns_hbar_daterange[1],
                     input$concerns_hbar_daterange[2]))
    }

    very_concise_concerns <-
      summarise(very_concise_concerns, count = sum(count), .by = concern)

    if(input$is_concerns_pie){
      plot_out <-
        mutate(very_concise_concerns,
               concern = fct_other(concern, keep = input$concerns_hbar_highlight)) |>
        summarise(count = sum(count), .by = concern) |>
        mutate(prop = count/sum(count)) |>

        ggplot(aes(x = 1, y = count, fill = concern)) +

        geom_col(position = "stack", colour = "black") +
        geom_text(aes(label = label_percent(0.1)(prop),
                      x = 1.6),
                  position = position_stack(0.5),
                  size = 6) +

        coord_polar("y") +

        xlab(NULL) +
        ylab(NULL) +
        theme_minimal() +
        theme(
          text = element_text(size = 20),
          panel.grid = element_blank(),
          axis.text = element_blank()
        )

    } else {

      plot_out <-
      filter(very_concise_concerns, concern %in% input$concerns_hbar_highlight) |>
        ggplot(aes(x = count, y = reorder(concern, count), fill = concern)) +

        geom_col() +

        labs(x = "Count",
             y = "Conversation topics",
             title = "What are we talking about?") +
        theme_ca("black") +
        theme(legend.position = "none",
              text = element_text(size = 28),
              panel.grid.major.y = element_blank())
    }

    plot_out +
      scale_fill_manual(values = fill_colours)

  })

  ## Pie chart people plot -----------------------------------------------------

  very_concise_people <-
    data |>
    pivot_longer(starts_with("people_"),
                 names_to = "people",
                 values_to = "indicated") |>
    summarise(count = sum(indicated),
              .by = c(month, people)) |>
    mutate(people =
             str_remove(people, "people_") |>
             capitalise() |>
             str_replace_all("_", " ") |>
             ordered())

  people_pie <-

    ggplot(very_concise_people, aes(x = 1, y = count, fill = people)) +

    geom_col(width = 0.7) +

    scale_fill_manual(values = rep(brewer.pal(12, "Set3"), length.out = nrow(very_concise_people))) +

    scale_x_continuous(breaks = 1, labels = "All People") +

    scale_x_discrete(labels =
                       \(x){
                         str_remove(x, "people")  |>
                           str_replace("wo_men", "women") |>
                           to_title_case()
                         }) +

    coord_polar("y") +

    theme_ca("black") +

    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "none")

  output$people_pie_chart <- renderPlot({people_pie})


}

#### RUN APP ###################################################################

shinyApp(ui, server)
