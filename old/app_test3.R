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

# set general vectors used

# all <- c("l1milking", "l2milking", "l3milking", "l4milking", "l5pmilking")
# one <- c("allmilking", "l2milking", "l3milking", "l4milking", "l5pmilking")
# two <- c("allmilking", "l1milking", "l3milking", "l4milking", "l5pmilking")
# three <- c("allmilking", "l2milking", "l1milking", "l4milking", "l5pmilking")
# four  <- c("allmilking", "l2milking", "l3milking", "l1milking", "l5pmilking")
# five <- c("allmilking", "l2milking", "l3milking", "l4milking", "l1milking")

# les_variables <- c("lesion", "dd", "footrot", "wld",
#                    "soleulcer", "solefract", "hem", "cork", 
#                    "other", "axial",
#                    "toeulcer", "thin", "inf", "noninf", "toe", "injury")

# farms <- "example"

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
  
  temp_data <- reactive({
    if (input$lactation == "all") {  
      lactation_value <- all   
      lact_group <- c(1:20)
    } else if(input$lactation == "1") {
      lactation_value <- one 
      lact_group <- c(1)
    } else if(input$lactation == "2") {
      lactation_value <- two 
      lact_group <- c(2)
    } else if(input$lactation == "3") {
      lactation_value <- three 
      lact_group <- c(3)
    } else if(input$lactation == "4") {
      lactation_value <- four 
      lact_group <- c(4)
    } else if(input$lactation == "5+") {
      lactation_value <- five 
      lact_group <- c(5:20)
    } else {
      lactation_value <- all  
      lact_group <- c(1:20)
    }
  
    region_lesion_sum_data(denominator = mall,
                                      data = lame4,
                                      lctgp = lactation_value, 
                                      lact = lact_group,
                                      group = "farm")  
  })
    
  output$temp_data_print <- renderPrint({
    print(temp_data())  # Call temp_data() with parentheses to access the value
  })

  # create graphs
  les_graph <- reactive({
    if (input$lactation == "all") {  
      lactation_value <- all   
      lact_group <- c(1:20)
    } else if(input$lactation == "1") {
      lactation_value <- one 
      lact_group <- c(1)
    } else if(input$lactation == "2") {
      lactation_value <- two 
      lact_group <- c(2)
    } else if(input$lactation == "3") {
      lactation_value <- three 
      lact_group <- c(3)
    } else if(input$lactation == "4") {
      lactation_value <- four 
      lact_group <- c(4)
    } else if(input$lactation == "5+") {
      lactation_value <- five 
      lact_group <- c(5:20)
    } else {
      lactation_value <- all  
      lact_group <- c(1:20)
    }
    
    region_lesion_sum_data(denominator = mall,
                                      data = lame4,
                                      lctgp = lactation_value,
                                      lact = lact_group,
                                      group = "farm") |> 
    region_les_graph(lesions = "lesion",
                     group = "farm",
                     plot_var = farm,
                     facet_col = year,
                     years = c(2023),
                     farms = c(farms)
                     )
  }
  )
  
  les_graph_inf <- region_lesion_sum_data(denominator = mall,
                                      data = lame4,
                                      lctgp = all, lact = c(1:20),
                                      group = "farm")  |>
    region_les_graph(lesions = c("footrot", "dd"),
                     group = "farm",
                     plot_var = farm,
                     facet_col = lestype,
                     years = c(2023),
                     farms = c(farms))
  
  les_graph_noninf <- region_lesion_sum_data(denominator = mall,
                                          data = lame4,
                                          lctgp = all, lact = c(1:20),
                                          group = "farm")  |>
    region_les_graph(lesions = c("soleulcer", "wld", "solefract"),
                     group = "farm",
                     plot_var = farm,
                     facet_col = lestype,
                     years = c(2023),
                     farms = c(farms))
  
  
  

  output$any <- renderPlot({
    plot <- les_graph()
    plot
    })
  output$inf <- renderPlot(les_graph_inf)
  output$noninf <- renderPlot(les_graph_noninf)


}


# Run the application 
shinyApp(ui = ui, server = server)
