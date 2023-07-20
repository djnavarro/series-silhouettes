
# load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(Rcpp)
library(e1071)
library(flametree)
library(ggforce)


silhouette <- function(seed, tree, background, cloud1, cloud2) {


  # constants ---------------------------------------------------------------

  system_id <- "07"
  system_name <- "silhouette"
  resolution <- 3000
  dpi <- 300


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

  as_hexcode <- function(name) {
    col_matrix <- grDevices::col2rgb(name)
    col_string <- grDevices::rgb(
      red   = col_matrix[1, ]/255,
      green = col_matrix[2, ]/255,
      blue  = col_matrix[3, ]/255
    )
    return(col_string)
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


  generate_ridge <- function(seed, id, n = 10000) {

    set.seed(seed)

    dat <- tibble(
      x = 1:n,
      y = c(rbridge(1, n)),
      seed = seed,
      id = id
    )

    for(i in 1:100) {
      end <- round(rbeta(1, 2, 2) * n)
      start <- round(rbeta(1, 2, 2) * n)
      if(start > end) {
        tmp <- start
        start <- end
        end <- tmp
      }
      m <- end - start
      dat$y[start:end] <- dat$y[start:end] + c(rbridge(1, m + 1))
    }

    dat$y <- dat$y + dbeta(dat$x/n, 2, 2) * 5

    dat <- dat %>%
      slice_head(n = n - n/5) %>%
      slice_tail(n = n - 2*n/5)

    return(dat)

  }

  generate_ridges <- function(n) {
    cat("generating ridges...\n")
    sample(1000, n) %>%
      imap_dfr(generate_ridge) %>%
      arrange(seed) %>%
      mutate(x = x - min(x)) %>%
      mutate(
        x = x/max(x),
        y = y/max(y)/4 + ((n-id)/n)/4
      )
  }

  # create data etc ---------------------------------------------------------

  shades <- c(tree, background, cloud1, cloud2) %>% map_chr(as_hexcode)
  background <- shades[2]
  tree_shade <- shades[1]
  leaf_shade <- shades[1]
  nebula_shades <- c(shades[3], blend_shades(shades[3], shades[4]), shades[4])

  nebula_data <- generate_nebula(10000000, seed) %>%
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

  ridge_data <- generate_ridges(15)


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

    geom_area(        # draw ridges next
      data = ridge_data,
      mapping = aes(
        x = x,
        y = y,
        group = id,
        fill = id
      ),
      size = 0,
      position = "identity",
      alpha = 1,
      show.legend = FALSE
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
    scale_fill_gradient(
      high = blend_shades(tree_shade, "black", .8),
      low = blend_shades(tree_shade, "black", .2)
    ) +
    coord_cartesian(xlim = c(.02, 1), ylim = c(.02, 1)) +
    NULL



  # render the image --------------------------------------------------------

  cat("rendering image...\n")
  spec <- paste(seed, shades[1], shades[2], shades[3], shades[4], sep="_") %>%
    str_remove_all("#")
  output_file <- paste0(system_name, "_", system_id, "_", spec, ".jpg")

  ggsave(
    plot = pic,
    filename = output_file,
    path = here("image"),
    width = resolution/dpi,
    height = resolution/dpi,
    dpi = dpi
  )

}

