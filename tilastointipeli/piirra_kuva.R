library(ggplot2)
library(cowplot)
library(magick)

#'
#' @examples
#' g <- piirra_kuva()
#' g
piirra_kuva <- function(point_size = 7, image_scale = .75) {
  
  data <- expand.grid(x = 1:6, y = 1:6)
  
  g <- ggplot(data, aes(x = x, y = y))
  
  for(i in 1:6)
    g <- g + geom_segment(x = i, y = 1, xend =i, yend = 6)
  g <- g + geom_point(shape = 21, size = point_size, fill = "white")
  g <- g + theme_void()
  g
  
  imgpath <- function(index, 
                      basepath = here::here("tilastointipeli/kuvat" ))
    paste0(basepath, "/noppa",index, ".png")
  
  pimage <- axis_canvas(g, axis = 'x')
  for(i in 1:6)
    pimage <- pimage + draw_image(imgpath(i), 
                                  x = i - 0.5, 
                                  scale = image_scale) 
  
  
  # insert the image strip into the plot
  ggdraw(insert_xaxis_grob(g, pimage, position = "bottom"))
  
}