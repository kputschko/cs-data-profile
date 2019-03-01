
# Data Profile ------------------------------------------------------------
# ggplot2 Shiny UI - https://github.com/gertstulp/ggplotgui/blob/master/R/ggplot_shiny.R

pacman::p_load(
  tidyverse,
  rlang,
  shiny,
  shinydashboard,
  DT,
  kp.helpers,
  GGally,
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
      box(h4(strong("Data Introduction")), width = 3, DTOutput("data_intro")),
      box(h4(strong("Data Summary")), width = 9, DTOutput("data_summary"))
    ), # Close Summary Row

    fluidRow(
      box(h4(strong("Correlation Plot")), width = 6, plotOutput("plot_corr", height = "550px")),
      box(h4(strong("Missing Values")), width = 6, plotOutput("plot_missing", height = "550px"))
    ), # Close Corr + Miss Plot

    fluidRow(
      box(h4(strong("Select Plot Axes")), width = 3,
          uiOutput("plot_x"),
          uiOutput("plot_y"),
          uiOutput("plot_facet"),
          actionButton("body_plot_button", "Create Plot", width = "75%")),

      box(width = 9,
          plotOutput("plot"))
    ) # Close Plot Row

  ) # Close Body

) # Close UI

# Server Logic ------------------------------------------------------------

server <- function(input, output) {

  # | Data Import ----
  data_raw <- eventReactive(input$sidebar_import_data, {
    input$sidebar_select_data %>% fx_helper_data()
  })

  output$data_intro <- renderDT({
    data_raw() %>%
      DataExplorer::introduce() %>%
      mutate_at("memory_usage", scales::number_bytes) %>%
      mutate_if(is.integer, scales::comma) %>%
      gather(name, value) %>%
      fx_helper_DT(scrollY = 400)
  })

  # | Data Summary ----
  output$data_summary <- renderDT({
    data_raw() %>%
      kp.helpers::fx_describe(output_format = "character") %>%
      fx_helper_DT(scrollY = 400)
    })


  # | Correlation ----
  output$plot_corr <- renderPlot({
    data_raw() %>%
      dummify() %>%
      ggcorr(nbreaks = 5, high = "#3B9AB2", low = "#F21A00")
  })

  # | Missing Values ----
  output$plot_missing <- renderPlot({
    data_raw() %>%
      plot_missing(ggtheme = theme_minimal())
  })

  # | Plot Select ----

  # column_names <- reactive(data_raw() %>% colnames())
  #
  # output$plot_x <- renderUI({
  #   fx_helper_shiny_select(id = "plot_x", label = "Select X", choices = column_names())
  # })
  #
  # output$plot_y <- renderUI({
  #   fx_helper_shiny_select(id = "plot_y", label = "Select Y", choices = column_names())
  # })
  #
  # # | Plot ----
  # output$plot_facet <- renderUI({
  #   fx_helper_shiny_select(id = "plot_facet", label = "Select Facet", choices = column_names())
  # })
  #
  # plot_temp <-
  #   eventReactive(input$body_plot_button, {
  #     if (input$plot_y %>% is.null()) {
  #       quickplot(data = data_raw(), x = input$plot_x, facets = input$plot_facet)
  #     } else {
  #       quickplot(data = data_raw(), x = input$plot_x, y = input$plot_y, facets = input$plot_facet)
  #     }
  #   })
  #
  # output$plot <- renderPlot({plot_temp()})

} # Close Server

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

