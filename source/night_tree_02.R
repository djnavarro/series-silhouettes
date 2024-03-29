
# load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(Rcpp)
library(flametree)
library(ggforce)


# constants ---------------------------------------------------------------

seeds <- 251:299
system_id <- "02"
system_name <- "night_tree"
background <- "#253035"
tree_shade <- "black"
leaf_shade <- "black"
resolution <- 3000
dpi <- 300


# helper functions --------------------------------------------------------

sourceCpp(here("source","nebula.cpp"))

generate_shades <- function(n, seed) {
  set.seed(seed)
  shades <- sample(colours(distinct = TRUE), n)
  return(shades)
}

generate_nebula <- function(n, seed) {
  set.seed(seed)
  cat("generating nebula...\n")
  nebula_data <- nebula(iterations = n)
  colnames(nebula_data) <- c("x0", "y0", "x1", "y1", "shade")
  nebula_data <- nebula_data %>%
    as_tibble() %>%
    slice(-(1:100)) %>%
    sample_frac(.03)
  return(nebula_data)
}

generate_tree <- function(n, seed) {
  cat("generating tree...\n")
  tree_data <- flametree_grow(
    seed = seed,
    time = n,
    scale = c(0.6, 0.9, 0.9),
    angle = c(10, -10, -20)
  ) %>%
    mutate(
      coord_x = 0.5 + coord_x / 8,
      coord_y = coord_y / 8
    )
  return(tree_data)
}


for(seed in seeds) {
  cat(seed, "\n")

  # create data etc ---------------------------------------------------------

  nebula_shades <- generate_shades(8, seed)
  nebula_data <- generate_nebula(10000000, seed)
  tree_data <- generate_tree(16, seed)
  leaf_data <- tree_data %>%
    filter(id_time >= 14) %>%
    sample_frac(.5)
  tree_data <- tree_data %>%
    filter(id_time <= 11)


  # create ggplot object ----------------------------------------------------

  cat("defining image...\n")
  pic <- ggplot() +
    geom_segment(
      data = nebula_data,  # draw nebula first
      mapping = aes(
        x = x0,
        y = y0,
        xend = x1,
        yend = y1,
        colour = shade
      ),
      show.legend = FALSE,
      size = .5,
      alpha = .2
    ) +
    geom_bezier(
      data = tree_data,  # draw tree trunk next
      mapping = aes(
        x = coord_x,
        y = coord_y,
        size = seg_wid * 8,
        group = id_path
      ),
      colour = tree_shade,
      show.legend = FALSE,
      lineend = "round",
      alpha = 1
    ) +
    geom_point(
      data = leaf_data,  # add leaves last
      mapping = aes(
        x = coord_x,
        y = coord_y
      ),
      size = 1,
      colour = leaf_shade,
      stroke = 0,
      alpha = 1,
      show.legend = FALSE
    ) +
    theme_void() +
    theme(panel.background = element_rect(
      fill = background,
      colour = background
    )) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_gradientn(colours = nebula_shades) +
    scale_size_identity() +
    coord_cartesian(xlim = c(.02, 1), ylim = c(.02, 1)) +
    NULL



  # render the image --------------------------------------------------------

  cat("rendering image...\n")
  output_file <- paste0(system_name, "_", system_id, "_", seed, ".jpg")

  ggsave(
    plot = pic,
    filename = output_file,
    path = here("image"),
    width = resolution/dpi,
    height = resolution/dpi,
    dpi = dpi
  )

}
