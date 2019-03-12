
# For PowerBI -------------------------------------------------------------

library(tidyverse)
library(DataExplorer)
library(broom)
library(kp.helpers)

options(warn = 0)

# Data --------------------------------------------------------------------

data_raw <- iris


# Introduction ------------------------------------------------------------

data_intro <- introduce(data_raw)


# Summary -----------------------------------------------------------------

data_summary <- fx_describe(data_raw)


# Missing -----------------------------------------------------------------

data_missing <- profile_missing(data_raw)


# Correlation -------------------------------------------------------------

d_cor <- data_raw %>% dummify() %>% cor()
d_tri <- lower.tri(d_cor) * d_cor

data_corr <-
  d_tri %>%
  as_tibble(rownames = "col_x") %>%
  gather(col_y, cor, -col_x) %>%
  mutate_at(vars(cor), ~ ifelse(. == 0, NA, .))


# PCA ---------------------------------------------------------------------

d_pca <- data_raw %>% dummify() %>% prcomp(scale = TRUE, center = TRUE)

data_pca <-
  d_pca %>%
  tidy(matrix = "variables") %>%
  filter(PC %in% c(1:2)) %>%
  mutate(PC = str_c("PC", PC)) %>%
  spread(PC, value)


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
  select(k, tot.withinss, recommended_clusters)
