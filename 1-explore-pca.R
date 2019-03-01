
# PCA ---------------------------------------------------------------------

# ggplot pca
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

# want biplot and scree plot

pacman::p_load(tidyverse, DataExplorer, ggfortify, MASS)

df <- iris %>% dummify()

df_pca <- prcomp(df, center = TRUE, )

df_pca <- princomp(df, scores = TRUE, cor = TRUE)

df_pca$scores %>% as_tibble()
df_pca$loadings

autoplot



data <- data.frame(obsnames=row.names(PC$x), PC$x)
plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
mult <- min(
  (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
  (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get(x)),
                    v2 = .7 * mult * (get(y))
)
plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
plot
}


library(MASS)
set.seed(6543)
n <- 500
mu <- c(1,-2,3,-1,3,4)
Sigma <- diag(rep(1,length(mu)))
Sigma[3,1] <- Sigma[1,3] <- 0.1
Sigma[4,6] <- Sigma[6,4] <- 0.1
X <- as.data.frame(mvrnorm(n, mu=mu, Sigma=Sigma))

# PCA
pca <- princomp(X, scores=T, cor=T)

# Scores
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# Loadings
loads <- pca$loadings

# Scale factor for loadings
scale.loads <- 5

# 3D plot
library(plotly)
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers",
            marker = list(color=y,
                          colorscale = c("#FFE1A1", "#683531"),
                          opacity = 0.7))

for (k in 1:nrow(loads)) {
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  z <- c(0, loads[k,3])*scale.loads
  p <- p %>% add_trace(x=x, y=y, z=z,
                       type="scatter3d", mode="lines",
                       line = list(width=8),
                       opacity = 1)
}
print(p)

df <- iris %>% dummify() %>% prcomp()

df_pca <- df %>% broom::tidy(matrix = "pcs")
df_sam <- df %>% broom::tidy(matrix = "samples")
df_var <- df %>% broom::tidy(matrix = "variables")

df %>% biplot()

df_var %>%
  filter(PC <= 2) %>%
  spread(PC, value) %>%
  ggplot(aes(x = `1`, y = `2`, label = column)) +
  geom_point() +
  # geom_label() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  lims(x = c(-1, 1), y = c(-1, 1))


df_dia <- diamonds %>% dummify() %>% prcomp()
df_dia_2 <- diamonds %>% dummify() %>% princomp(cor = TRUE)

df_dia_2$loadings

df_dia_var <- df_dia %>% broom::tidy(matrix = "variables")
df_dia_pca <- df_dia %>% broom::tidy(matrix = "pcs")

df_dia_var
df_dia_pca

df_dia_var %>%
  filter(PC <= 2) %>%
  spread(PC, value) %>%
  ggplot(aes(x = `1`, y = `2`, label = column)) +
  geom_point() +
  # geom_label() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  lims(x = c(-1, 1), y = c(-1, 1))

df_dia %>% plot_cor

