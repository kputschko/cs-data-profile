
fx_helper_plot <- function(data, type, x = "NULL", y = "NULL", by = "NULL", color = "NULL", size = "NULL") {

  pacman::p_load(tidyverse, rlang)

  r_x <- parse_quo(x, caller_env())
  r_y <- parse_quo(y, caller_env())
  r_by <- parse_quo(by, caller_env())
  r_color <- parse_quo(color, caller_env())
  r_size <- parse_quo(size, caller_env())

  r_plot_geom <- switch(type,
                        bar = geom_bar(),
                        scatter = geom_point(),
                        density = geom_density(),
                        histogram = geom_histogram())

  ggplot2::theme_set(kp.helpers::theme_kp())



  if (y == "NULL") {
    ggplot(data = data, mapping = aes(x = !!r_x, color = !!r_color, size = !!r_size)) + r_plot_geom
  } else {
    ggplot(data = data, mapping = aes(x = !!r_x, y = !!r_y, color = !!r_color, size = !!r_size)) + r_plot_geom
  }

  # ggplot(data = data,
  #        mapping = aes(x = !!r_x, y = !!r_y)) +
  #   geom_point()


  # plot_base <- ggplot(data = data) + aes(x = !!r_x, y = !!r_y, color = !!r_color, size = !!r_size)

  # plot_type <- switch(type,
  #                     bar = geom_bar(),
  #                     scatter = geom_point(),
  #                     density = geom_density(),
  #                     histogram = geom_histogram())
  #
  # plot_base + plot_type

}


# debugonce(fx_helper_plot)
fx_helper_plot(mtcars, "histogram", x = "cyl")
fx_helper_plot(mtcars, "scatter", x = "cyl", y = "mpg", color = "cyl")



# With qplot --------------------------------------------------------------

qplot(x = carat, data = diamonds, facets = NULL) + theme_kp()
qplot(x = "x", y = "y", data = diamonds, facets = "color") + theme_kp()
