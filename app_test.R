#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# add R folder and put scripts/functions in that

# load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,           # import/export
  janitor,       # tables
  tidyverse,     # data mgmt and viz
  zoo,           # dates
  gtsummary,     # tables
  dtplyr,        # to speed up code
  ggtext,       # dim graph
  ragg,         # dim graph
  knitr,         # tables
  epiR,          # Conf Inf
  ggridges,       # for ridgeplots
  shiny,
  bslib
)

# load data
# denominators
mall <- import("data/mall.rds", trust = TRUE) 
# data
lame4 <-import("data/lame4.rds", trust = TRUE)

# load functions
source("R/fxn_lesions_graphs.R")

# set up

ui <- page_sidebar( 
  title = "Hoof Lesion Data Analysis App", 
  theme = bs_theme(base_font = font_google("Outfit"),
                   preset = "simplex", bg = "white", fg = "#7a0019",
                   primary = "grey"), 
  
  sidebar = sidebar(
    h4("Options"),
    hr(),
    selectInput(inputId = "Parameter",
                label = "Select graphing parameter",
                c("Overall Incidence", "DIM", "Season"),
    ),
    hr(),
    radioButtons(inputId = "lactation", 
                 label = "Select Lactation:",
                choices = c("All" = "all" , "1", "2", "3+")
                ),
    hr(),
    dateRangeInput(inputId = "date", label = "Analysis Date Range "),
    hr(),
    verbatimTextOutput("date"),
    verbatimTextOutput("date1"),
    verbatimTextOutput("date2"),
    
    verbatimTextOutput("date1_class"),
    verbatimTextOutput("date1_year")
  ),
  
  navset_tab(
  nav_panel("Any Lesions", plotOutput("any")), 
  nav_panel("Infectious Lesions", plotOutput("inf")), 
  nav_panel("Non Infectious Lesions", plotOutput("noninf")), 
  
  # Place additional UI elements properly
  header = uiOutput("valueboxes"),
  
  # Use `tags$div` for image styling correctly
  footer = tags$div(
    img(src = "umn_logo.jpg",
        width = "80px",
        height = "96px",
        style = "position: fixed; bottom: 
        24px; right: 24px; opacity: 0.75; z-index: 1;"
    )
  )
  )
)


server <- function(input, output, session) {
  
  # Use the les_graph with direct mapping
  les_graph <- reactive({
    # Directly determine the mapped value based on the input
    mapped_value <- if(input$lactation == "All") {
      all
    } else if(input$lactation == "1") {
      one
    } else if(input$lactation == "2") {
      two
    } else if(input$lactation == "3+") {
      three
    } else {
      all  # Default case
    }
    
    region_lesion_sum_data(
      denominator = mall,
      data = lame4,
      lctgp = mapped_value, 
      lact = c(1:20),
      group = "farm"
    ) |>
      region_les_graph(
        lesions = "lesion",
        group = "farm",
        plot_var = farm,
        facet_col = year,
        years = c(2023),
        farms = c(farms)
      )
  })
  
  # For the infectious lesions graph
  les_graph_inf <- reactive({
    # Directly determine the mapped value based on the input
    mapped_value <- if(input$lactation == "All") {
      all
    } else if(input$lactation == "1") {
      one
    } else if(input$lactation == "2") {
      two
    } else if(input$lactation == "3+") {
      three
    } else {
      all  # Default case
    }
    
    region_lesion_sum_data(
      denominator = mall,
      data = lame4,
      lctgp = mapped_value,
      lact = c(1:20),
      group = "farm"
    ) |>
      region_les_graph(
        lesions = c("footrot", "dd"),
        group = "farm",
        plot_var = farm,
        facet_col = lestype,
        years = c(2023),
        farms = c(farms)
      )
  })
  
  # For the non-infectious lesions graph
  les_graph_noninf <- reactive({
    # Directly determine the mapped value based on the input
    mapped_value <- if(input$lactation == "All") {
      all
    } else if(input$lactation == "1") {
      one
    } else if(input$lactation == "2") {
      two
    } else if(input$lactation == "3+") {
      three
    } else {
      all  # Default case
    }
    
    region_lesion_sum_data(
      denominator = mall,
      data = lame4,
      lctgp = mapped_value,
      lact = c(1:20),
      group = "farm"
    ) |>
      region_les_graph(
        lesions = c("soleulcer", "wld", "solefract"),
        group = "farm",
        plot_var = farm,
        facet_col = lestype,
        years = c(2023),
        farms = c(farms)
      )
  })
  
  # Render plots
  output$any <- renderPlot(les_graph)
  output$inf <- renderPlot(les_graph_inf)
  output$noninf <- renderPlot(les_graph_noninf)
}

# Run the application 
shinyApp(ui = ui, server = server)
