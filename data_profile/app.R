#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(
  tidyverse,
  rlang,
  shiny,
  shinydashboard,
  DT,
  kp.helpers,
  nycflights13,
  gapminder,
  spData,
  DataExplorer)

data_choices <-
  c("mtcars",
    "iris",
    "airlines",
    "airports",
    "flights",
    "planes",
    "weather",
    "boston.c",
    "airquality",
    "landslide",
    "diamonds")

inform(str_c("Application launched at ", Sys.time()))


# Helper Functions --------------------------------------------------------

fx_helper_data <- function(selection) {
  if (selection == "landslide") {
    df_temp <- read_csv("https://raw.githubusercontent.com/frm1789/landslide-ea/master/global_landslide_catalog_export.csv")
    if (!all(dim(df_temp) == c(11033, 31))) {warn(str_glue("data '{selection}' is not currently available"))}
  } else {
    df_temp <- selection %>% rlang::parse_expr() %>% rlang::eval_tidy()
    if (!is.data.frame(df_temp)) {warn(str_glue("data '{selection}' is not currently available"))}
  }
  return(df_temp)
}

fx_helper_DT <- function(x, scrollY = 200) {
  datatable(data = x,
            extensions = c('Scroller'),
            rownames = FALSE,
            options = list(dom = 't', scrollY = scrollY, scroller = TRUE, scrollX = TRUE))
}

fx_helper_shiny_select <- function(id, label, choices) {
  selectizeInput(inputId = id,
                 label = label,
                 multiple = TRUE,
                 selected = NULL,
                 choices = choices %>% sort(),
                 options = list(maxItems = 1, placeholder = "NULL")
  )
}


# User Interface ----------------------------------------------------------

ui <- dashboardPage(

  # | Header ----
  header = dashboardHeader(title = "Data Profile"),

  # | Sidebar ----
  sidebar = dashboardSidebar(

    fx_helper_shiny_select(id = "sidebar_select_data",
                           label = "Select Data",
                           choices = data_choices),
    hr(),
    actionButton("sidebar_import_data", "Import Data", width = "75%")

  ), # Close Sidebar

  # | Body ----
  body = dashboardBody(

    fluidRow(
      box(h4(strong("Data Summary")), width = 10, DTOutput("data_summary"))
    )

  ) # Close Body

) # Close UI

# Server Logic ------------------------------------------------------------

server <- function(input, output) {

  # | Data Import ----
  data_raw <- eventReactive(input$sidebar_import_data, {
    input$sidebar_select_data %>% fx_helper_data()
  })

  # | Data Summary ----
  output$data_summary <- renderDT({
    data_raw() %>%
      kp.helpers::fx_describe(output_format = "numeric") %>%
      fx_helper_DT(scrollY = 400)
    })

} # Close Server

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

