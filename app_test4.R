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
                choices = c("all", "1", "2", "3", "4", "5+")
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
  nav_panel("test", verbatimTextOutput("temp_data_print")),
  
  tags$style(HTML(
    ".navbar { 
        background-image: url('chute_small.jpg');
        background-size: cover;
        background-position: top;
      }"
  )),
  
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
  #bs_themer()
  # set variables used for functions
  
    # Create a mapping for lactation values
    lactation_mapping <- list(
      all = list(lactation_value = all, lact_group = c(1:20)),
      "1" = list(lactation_value = one, lact_group = c(1)),
      "2" = list(lactation_value = two, lact_group = c(2)),
      "3" = list(lactation_value = three, lact_group = c(3)),
      "4" = list(lactation_value = four, lact_group = c(4)),
      "5+" = list(lactation_value = five, lact_group = c(5:20))
    )
    
    # Create the reactive expression to handle the lactation input
    graph_data <- reactive({
      lactation_info <- lactation_mapping[[input$lactation]]  # Use input$lactation to get the corresponding values
      lactation_value <- lactation_info$lactation_value
      lact_group <- lactation_info$lact_group
      
      region_lesion_sum_data(denominator = mall,
                             data = lame4,
                             lctgp = lactation_value, 
                             lact = lact_group,
                             group = "farm")  
    })
    
    output$temp_data_print <- renderPrint({
      print(graph_data())  # Call temp_data() with parentheses to access the value
    })
    
  output$any <- renderPlot({
    les_graph <- region_les_graph(.data = graph_data(), # need to use () as it's a funtion
                                             lesions = c("lesion", 
                                                         "inf", "noninf"),
                                             group = "farm",
                                             plot_var = lestype,
                                             facet_col = year,
                                             years = c(2023, 2024),
                                             farms = c(farms))
    les_graph
    })
  output$inf <- renderPlot({
    les_graph_inf <- region_les_graph(.data = graph_data(), # need to use () as it's a funtion
                                      lesions = c("footrot", "dd"),
                                      group = "farm",
                                      plot_var = lestype,
                                      facet_col = year,
                                      years = c(2024),
                                      farms = c(farms))
    les_graph_inf
  })
  output$noninf <- renderPlot({
    les_graph_noninf <- region_les_graph(.data = graph_data(), # need to use () as it's a funtion
                                      lesions = c("soleulcer", "solefract", "wld"),
                                      group = "farm",
                                      plot_var = lestype,
                                      facet_col = year,
                                      years = c(2024),
                                      farms = c(farms))
    les_graph_noninf
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
