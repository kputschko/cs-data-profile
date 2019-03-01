
# Correlation Plot --------------------------------------------------------

pacman::p_load(tidyverse, corrplot, DataExplorer, GGally)

mtcars %>% DataExplorer::dummify()
diamonds %>% DataExplorer::dummify()

diamonds %>% DataExplorer::dummify() %>% corrplot("ellipse", "upper", is.corr = FALSE)
diamonds %>% DataExplorer::dummify() %>% cor(use = "na.or.complete") %>% corrplot("ellipse", "upper")

# select_if(is.numeric) %>%
#   cor(use = "na.or.complete") %>%
#   corrplot::corrplot("ellipse", "upper")

diamonds %>% DataExplorer::dummify() %>% ggcorr()


diamonds %>%
  DataExplorer::dummify() %>%
  cor(use = "na.or.complete") %>%
  as_tibble(rownames = "column_1") %>%
  gather(column_2, corr, -column_1) %>%
  ggplot() +
  aes(x = column_1, y = column_2, fill = corr) +
  geom_tile(size = 0.25, color = "white") +
  scale_fill_viridis_c(option = "D")




diamonds %>%
  DataExplorer::dummify() %>%
  cor(use = "na.or.complete") %>%
  magrittr::multiply_by(., lower.tri(.)) %>%
  as_tibble(rownames = "col_1") %>%
  gather(col_2, cor, -col_1) %>%
  filter(cor != 0) %>%
  ggplot() +
  aes(x = col_1, y = col_2, fill = cor) +
  geom_tile() +
  scale_fill_viridis_c(option = "A")


ggcorr

df     <- diamonds %>% dummify()
df_cor <- df %>% cor()
df_lt  <- df_cor * lower.tri(df_cor)
df_tbl <- df_lt %>% as_tibble(rownames = "col_1")
df_lng <- df_tbl %>% gather(col_2, coef, -col_1)


df_cor %>%
  arrange(col_1, col_2) %>%
  mutate(f = str_c(col_1, col_2, sep = "_"),
         b = str_c(col_2, col_1, sep = "_")) %>%
  filter(col_1 %in% c("carat", "color_D"),
         col_2 %in% c("carat", "color_D"),
         coef != 0)


df_na  <- df_lng %>% filter(coef != 0)

df_na %>%
  ggplot() +
  aes(x = col_1, y = col_2, fill = coef) +
  geom_tile(color = "white")


df_cor %>% ggcorr(nbreaks = 5, high = "#3B9AB2", low = "#F21A00")

v_cor <- cor(df, use = "complete.obs")
corrplot(v_cor, method="square",type = "upper", order = "hclust",
         tl.col = "black")
