library(ggplot2)
library(cowplot)
library(magick)

#'
#' @examples
#' g <- piirra_kuva()
#' g
piirra_kuva <- function(point_size = 7, image_scale = .75, n_point = 6) {
  
  data <- expand.grid(x = 1:n_point, y = 1:n_point)
  
  background <- png::readPNG(here::here("tilastointipeli/kuvat/grey-arrow.png"))
  g <- ggplot(data, aes(x = x, y = y))
  g <- g + ggpubr::background_image(background)
  
  for(i in 1:n_point)
    g <- g + geom_segment(x = i, y = 1, xend =i, yend = n_point)
  g <- g + geom_point(shape = 21, size = point_size, fill = "white")
  g <- g + theme_void()
  g
  
  imgpath <- function(index, 
                      basepath = here::here("tilastointipeli/kuvat" ))
    paste0(basepath, "/noppa",index, ".png")
  
  pimage <- axis_canvas(g, axis = 'x')
  for(i in 1:n_point)
    pimage <- pimage + draw_image(imgpath(i), 
                                  x = i - 0.5, 
                                  scale = image_scale) 
  
  
  # insert the image strip into the plot
  ggdraw(insert_xaxis_grob(g, pimage, position = "bottom"))
  
}

