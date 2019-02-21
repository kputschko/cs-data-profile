
# Data Entry Accuracy  ----------------------------------------------------

pacman::p_load(tidyverse, mlbench, kp.helpers)

pacman::p_load(outliers)


# Data --------------------------------------------------------------------

data(BostonHousing)
data <- BostonHousing %>% as_tibble()

data %>% glimpse()

# Manual Outlier ----------------------------------------------------------

data %>% add_row(crim = 1, .before = 1)

# IQR Function ------------------------------------------------------------

data %>% fx_describe()

# fx_iqr <-

# Outlier - Univariate ----------------------------------------------------

fx_iqr <- function(data, na.rm = TRUE, type = 7) {

  data %>% summarise_if(is_double, ~IQR(., na.rm = TRUE))

}

fx_outlier_uni <- function(data, ..., .before = 1) {

  iqr <- data %>% fx_iqr()
  parse_dots <- c(...)
  data_names <- names(parse_dots)
  iqr_outliers <- iqr * 1.5
  data_output <- data %>% add_row(... = ..., .before = .before)

  lst(iqr, parse_dots, data_names, iqr_outliers, data_output)

}

# Test
fx_outlier_uni(data, crim = c(1, 0), zn = -100)


# 1 - Assume Existing Data is Accurate -------------------------------------

data %>% glimpse()
data %>% fx_describe()

# 2 - Scan Incoming Data --------------------------------------------------

new_record <- c(crim = 1, zn = 100, indus = 5.5) %>% print()
data %>% add_row(!!!new_record, .before = 1)

outliers::outlier



# 3 - Assess Existing Data ------------------------------------------------
# The goal here is to give an ELT Consultant a place to start when asking questions
# of the data

# Sources:
# https://briatte.github.io/ggcorr/
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

pacman::p_load(tidyverse, kp.helpers, GGally, corrplot)

data <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

data %>% glimpse()
data %>% ggcorr()
data %>% ggcorr(geom = "circle")

data %>% fx_describe("numeric")



# Outliers ----------------------------------------------------------------

# pacman::p_load(tidyverse, TeachingDemos)


# DataExplore -------------------------------------------------------------
# https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html


pacman::p_load(tidyverse, DataExplorer, nycflights13)

merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)
merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, suffixes = c("_flights", "_planes"))
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin", by.y = "faa", all.x = TRUE, suffixes = c("_carrier", "_origin"))
final_data <- merge(merge_airports_origin, airports, by.x = "dest", by.y = "faa", all.x = TRUE, suffixes = c("_origin", "_dest"))

final_data %>% plot_str()
final_data %>% plot_str(type = "r")

final_data %>% introduce() %>% unlist() %>% enframe()
final_data %>% plot_intro()

final_data %>% profile_missing()
final_data %>% plot_missing()

final_data %>% glimpse()
final_data %>% plot_bar()
final_data$manufacturer %>% plot_bar()

final_data %>% plot_bar(with = "arr_delay")


final_data %>% plot_histogram()

config <- list(
  "introduce" = list(),
  "plot_str" = list(
    "type" = "diagonal",
    "fontSize" = 35,
    "width" = 1000,
    "margin" = list("left" = 350, "right" = 250)
  ),
  "plot_missing" = list(),
  "plot_histogram" = list(),
  "plot_qq" = list(sampled_rows = 1000L),
  "plot_bar" = list(),
  "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
  "plot_prcomp" = list(),
  "plot_boxplot" = list(),
  "plot_scatterplot" = list(sampled_rows = 1000L)
)
## Create final report
create_report(final_data, y = "arr_delay", config = config)
