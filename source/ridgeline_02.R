# create banner
library(tidyverse)
library(e1071)

seed <- 200

generate_ridge <- function(seed) {

  set.seed(seed)

  n <- 100000
  dat <- tibble(
    x = 1:n,
    y = c(rbridge(1, n)),
    seed = seed
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
    slice_head(n = n - 20000) %>% 
    slice_tail(n = n - 40000)
  
  return(dat)

}

seeds <- sample(1000, 20)
ridges <- map_dfr(seeds, generate_ridge) %>% 
  arrange(seed)


pic <- ggplot(ridges, aes(x,y, group = seed, fill = rank(seed))) +
  geom_area(size = 0, position = "identity", alpha = 1, show.legend = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "#333333",
    colour = "#333333"
  )) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scico::scale_fill_scico(palette = "lajolla", direction = 1) +
  coord_cartesian(ylim = c(0, max(ridges$y) * 1.3)) + 
  NULL
  
ggsave(
  filename = here::here("image", paste0("ridgeline_02_", seed, ".png")),
  plot = pic,
  width = 5000/300,
  height = 1000/300,
  dpi = 300
)
