
# load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(Rcpp)
library(flametree)
library(ggforce)


# constants ---------------------------------------------------------------

seeds <- 468
system_id <- "04"
system_name <- "silhouette_montage"
resolution <- 16000
dpi <- 1600




# helper functions --------------------------------------------------------

sourceCpp(here("source","nebula.cpp"))

blend_shades <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255,
           green = z[2, ]/255,
           blue = z[3, ]/255)
  return(z)
}

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
    sample_frac(.05)
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

  shades <- generate_shades(3, seed)
  background <- blend_shades(shades[1], "black", .8)
  tree_shade <- blend_shades(shades[1], "black")
  leaf_shade <- blend_shades(shades[1], "black")
  nebula_shades <- c(shades[2], blend_shades(shades[2], shades[3]), shades[3])

  nebula_data <- generate_nebula(20000000, seed) %>%
    mutate(exact_shade = case_when(
      shade < .33 ~ nebula_shades[1],
      shade < .67 ~ nebula_shades[2],
      shade < 1 ~ nebula_shades[3],
      TRUE ~ NA_character_
    ))
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
        colour = exact_shade
      ),
      show.legend = FALSE,
      size = 1,
      alpha = 1
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
    scale_colour_identity() +
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
