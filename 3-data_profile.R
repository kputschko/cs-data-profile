
# Data Profiles -----------------------------------------------------------

# Our output will be PowerBI

# We will have:
# - Selection of data (with table joins, filters, etc.)
# - Initial view of data size (in bytes and in dimension)
# - Quick summaries of each column (mean, min, max, etc.)
# - Correlations of one column to a single other column (not multivariate)
# - Principal components (to view how groups of columns relate to each other; multivariate)
# - Cluster analysis to see how rows group together (rows, or users, or IDs)

# NO SHINY :(

# In PowerBI
# - Interactive graph (select axes for graphs, histograms, boxplots, etc.)


pacman::p_load(tidyverse, rlang, DataExplorer, broom, GGally, kp.helpers)



# Load Data ---------------------------------------------------------------

fx_profile_load_data <- function(source) {
  if (source == "landslide") {
    df_temp <- read_csv("https://raw.githubusercontent.com/frm1789/landslide-ea/master/global_landslide_catalog_export.csv")
    if (!all(dim(df_temp) == c(11033, 31))) {warn(str_glue("data '{selection}' is not currently available"))}

  } else {
    df_temp <- source %>% rlang::parse_expr() %>% rlang::eval_tidy()
    if (!is.data.frame(df_temp)) {warn(str_glue("data '{source}' is not currently available"))}
  }

  if (!is_tibble(df_temp)) as_tibble(df_temp) else df_temp
}

data_sources <-
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

data_raw <- fx_profile_load_data("landslide") %>% print()
data_raw <- fx_profile_load_data("iris") %>% print()


# Byte Size ---------------------------------------------------------------

data_intro <- data_raw %>% DataExplorer::introduce()


# Summarise ---------------------------------------------------------------

data_summary <- data_raw %>% kp.helpers::fx_describe()


# Missing Values ----------------------------------------------------------

data_missing <- data_raw %>% DataExplorer::profile_missing()


# Correlation -------------------------------------------------------------

d_cor <- data_raw %>% dummify() %>% cor()
d_tri <- lower.tri(d_cor) * d_cor

data_corr <-
  d_tri %>%
  as_tibble(rownames = "col_x") %>%
  gather(col_y, cor, -col_x) %>%
  mutate_at(vars(cor), ~ ifelse(. == 0, NA, .))


# PCA ---------------------------------------------------------------------
# TO DO: Provide consice way of interpreting this plot
# How do points on biplot differ from cluster analysis?
# What do arrows represent?

d_pca <- data_raw %>% dummify() %>% prcomp(scale = TRUE, center = TRUE)

data_pca <-
  d_pca %>%
  tidy(matrix = "pcs") %>%
  mutate(trust = cut(cumulative,
                     breaks = c(0, .7, .85, .9, 1),
                     labels = c("low", "fair", "high", "high")))

data_pca_biplot <-
  d_pca %>%
  tidy(matrix = "variables") %>%
  filter(PC %in% c(1:2)) %>%
  mutate(PC = str_c("PC", PC)) %>%
  spread(PC, value)

# d_pca %>% tidy(matrix = "samples")
# d_pca %>% tidy(matrix = "variables")

# d_pca %>% biplot()
# d_pca %>% screeplot()

d_pca %>%
  tidy(matrix = "variables") %>%
  filter(PC %in% c(1:2)) %>%
  mutate(PC = str_c("PC", PC)) %>%
  spread(PC, value) %>%
  ggplot(aes(x = `PC1`, y = `PC2`, label = column)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2)) +
  geom_text(check_overlap = TRUE, nudge_y = 0.10) +
  lims(x = c(-1, 1), y = c(-1, 1))

# Cluster -----------------------------------------------------------------

d_clu <- data_raw %>% dummify() %>% scale()

data_cluster <-
  tibble(k = 1:20) %>%
  mutate(kclust = map(k, ~kmeans(d_clu, .x)),
         glance = map(kclust, glance)) %>%
  select(-kclust) %>%
  unnest(glance) %>%
  arrange(k) %>%
  mutate(rate = 100 * (tot.withinss - lag(tot.withinss)) / lag(tot.withinss)) %>%
  mutate(mark_potential = ifelse(rate < lag(rate), 1, 0)) %>%
  group_by(mark_potential) %>%
  mutate(mark_count = sequence(n()),
         recommended_clusters = ifelse(mark_potential == 1 & mark_count == 1, k, NA)) %>%
  ungroup() %>%
  select(k, tot.withinss, recommended_clusters) %>%
  print()

data_cluster %>%
  ggplot(aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = recommended_clusters, color = "red")) +
  theme_kp() +
  guides(color = "none") +
  scale_x_continuous(breaks = 1:20)


# Export for PowerBI ------------------------------------------------------

ls(pattern = "data_") %>%
  map(~mget(., inherits = TRUE)) %>%
  flatten() %>%
  keep(is.tibble) %>%
  enframe() %>%
  mutate(
    walk2(name, value, ~write_csv(.y, path = str_glue("temp_data/{.x}.csv")))
  )
