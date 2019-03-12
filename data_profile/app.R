
# Data Profile ------------------------------------------------------------
# ggplot2 Shiny UI - https://github.com/gertstulp/ggplotgui/blob/master/R/ggplot_shiny.R

library(tidyverse)
library(rlang)
library(broom)
library(scales)
library(shiny)
library(shinydashboard)
library(DT)
library(kp.helpers)
library(corrplot)
library(RColorBrewer)
library(spData)
library(DataExplorer)


data_choices <-
  c("mtcars",
    "iris",
    "weather",
    "boston.c",
    "airquality",
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
      box(h4(strong("Missing Values")), width = 6, plotOutput("plot_missing", height = "550px")),
      box(h4(strong("Correlation Plot")), width = 6, plotOutput("plot_corr", height = "550px"))
    ), # Close Corr + Miss Plot

    fluidRow(
      box(h4(strong("Principle Components")), width = 6,
          actionButton("plot_pca", label = "Find Principal Components"),
          hr(),
          plotOutput("plot_pca", height = "550px")),

      box(h4(strong("Clusters")), width = 6,
          actionButton("plot_cluster", label = "Find Clusters"),
          hr(),
          plotOutput("plot_cluster", height = "550px"))
    ) # Close PCA + Clusters

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
  data_summary <- reactive({
    data_raw() %>%
      fx_describe(output_format = "numeric")
  })

  output$data_summary <- renderDT({
      data_summary() %>% fx_helper_DT(scrollY = 400)
    })

  # | Missing Values ----
  data_missing <- reactive({
    data_summary() %>%
      select(column_name, pct_missing) %>%
      filter(pct_missing > 0) %>%
      arrange(pct_missing)
  })

  output$plot_missing <- renderPlot({

    .plot_missing_label <-
      if (nrow(data_missing()) == 0) {"Note: No missing data!"} else {NULL}

    data_missing() %>%
      ggplot(aes(x = reorder(column_name, pct_missing), y = pct_missing, fill = pct_missing)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(guide = "none") +
      labs(y = "Percent Missing", x = "Column", subtitle = .plot_missing_label) +
      theme_kp()
  })

  # | Correlation ----
  output$plot_corr <- renderPlot({
    data_raw() %>%
      dummify() %>%
      cor() %>%
      corrplot(
        tl.col = "black",
        type = "upper",
        col = brewer.pal(n = 8, name = "RdYlBu"),
        order = "AOE"
      )
  })

  # | PCA ----

  data_pca <- eventReactive(input$plot_pca, {
    data_raw() %>%
      dummify() %>%
      prcomp(scale = TRUE, center = TRUE)
  })

  data_pca_summary <- reactive({
    data_pca() %>%
      tidy(matrix = "pcs") %>%
      select(PC, percent) %>%
      filter(PC %in% 1:2) %>%
      deframe() %>%
      map_chr(percent, accuracy = 0.01)
  })


  output$plot_pca <- renderPlot({
    data_pca() %>%
      tidy(matrix = "variables") %>%
      filter(PC %in% c(1:2)) %>%
      mutate(PC = str_c("PC", PC)) %>%
      spread(PC, value) %>%
      ggplot(aes(x = PC1, y = PC2, label = column)) +
      geom_point() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2)) +
      geom_text(check_overlap = TRUE, nudge_y = 0.10) +
      theme_kp() +
      labs(title = "Principal Component Bi-Plot",
           caption = "If Percent of Variance Explained by PC1 + PC2 is > 80%, then this will be a trustworthy plot.\nOtherwise, more exploration of the components will be necessary.") +
      scale_x_continuous(labels = NULL,
                         limits = c(-1, 1),
                         name = str_glue("PC1 ({data_pca_summary()[1]})")) +
      scale_y_continuous(labels = NULL,
                         limits = c(-1, 1),
                         name = str_glue("PC2 ({data_pca_summary()[2]})"))


  })

  # | Cluster ----

  data_raw_cluster <- eventReactive(input$plot_cluster, {
    data_raw() %>% dummify() %>% scale()
  })

  # Determine optimal number of clusters
  data_cluster <- reactive({
    tibble(k = 1:20) %>%
      mutate(kclust = map(k, ~kmeans(data_raw_cluster(), .x)),
             glance = map(kclust, glance)) %>%
      unnest(glance) %>%
      arrange(k) %>%
      mutate(rate = 100 * (tot.withinss - lag(tot.withinss)) / lag(tot.withinss)) %>%
      mutate(mark_potential = ifelse(rate < lag(rate), 1, 0)) %>%
      group_by(mark_potential) %>%
      mutate(mark_count = sequence(n()),
             recommended_clusters = ifelse(mark_potential == 1 & mark_count == 1, k, NA)) %>%
      ungroup() %>%
      select(k, tot.withinss, recommended_clusters, kclust)
  })

  plot_cluster <- reactive({
    data_cluster() %>%
      filter(!is.na(recommended_clusters)) %>%
      mutate(plot_kmeans = map(kclust,
                               ~autoplot(., data_raw_cluster(), frame = TRUE, frame.type = "norm") +
                                 labs(title = "PCA - Clusters") +
                                 theme_kp()))
  })

  output$plot_cluster <- renderPlot(
    plot_cluster()$plot_kmeans[[1]]
  )


} # Close Server

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

