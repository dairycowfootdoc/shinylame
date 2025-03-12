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
mall <- import("mall.rds", trust = TRUE) 
# data
lame4 <-import("lame4.rds", trust = TRUE)

# set general vectors used
# used in lesion function
all <- c("l1milking", "l2milking", "l3milking", "l4milking", "l5pmilking")
one <- c("allmilking", "l2milking", "l3milking", "l4milking", "l5pmilking")
two <- c("allmilking", "l1milking", "l3milking", "l4milking", "l5pmilking")
three <- c("allmilking", "l2milking", "l1milking", "l4milking", "l5pmilking")
four  <- c("allmilking", "l2milking", "l3milking", "l1milking", "l5pmilking")
five <- c("allmilking", "l2milking", "l3milking", "l4milking", "l1milking")

les_variables <- c("lesion", "dd", "footrot", "wld",
                   "soleulcer", "solefract", "hem", "cork", 
                   "other", "axial",
                   "toeulcer", "thin", "inf", "noninf", "toe", "injury")

farms <- "example"


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
                choices = c("All" , "1", "2", "3+")
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
  nav_panel("Infectious Lesions", "Page B content"), 
  nav_panel("Non Infectious Lesions", "Page C content"), 
  
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


server <- function(input, output) {
  #bs_themer()
  # create graphs
  les_graph <- region_lesion_sum_data(lctgp = all, lact = c(1:20),
                                      group = "farm")  |>
    region_les_graph(lesions = "lesion",
                     group = "farm",
                     plot_var = farm,
                     facet_col = lestype,
                     years = c(2023),
                     farms = c(farms))
  
  output$any <- renderPlot(les_graph)
                        
                          
  
}


# Run the application 
shinyApp(ui = ui, server = server)
