#' Graphics for Linköping University
#' A ggplot2 theme for LiU. Recommend to render plot by cairo_pdf device in 4 x 6 dimensions.
#' @param
#' 
# 
data(iris)
require(ggplot2)
library(png)
library(extrafont)


# Read the png
liu_logo <- grid::rasterGrob(png::readPNG("liu_logo.png"), interpolate = TRUE)

LiU_theme <- function(){
  theme(plot.title = element_text(hjust=0.5, face="bold", size=14, family="Tahoma"), # Title
        axis.title = element_text(face="bold", size=12, family="Tahoma"),            # Axis title
        axis.text = element_text(size=10, family="Tahoma"),                          # Axis text
        plot.margin = unit(c(1, 1, 3, 1), "lines"),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid = element_line(colour = "grey85"),
        panel.background = element_rect(fill = "white", colour = NA)
  )
}

add_LiU_logo <- function(plot){
  graph_data<-ggplot_build(p)$data[[1]]
  x_max <- max(graph_data$x)
  x_min <- min(graph_data$x)
  y_min <- min(graph_data$y)
  plot <- plot + annotation_custom(liu_logo,
                                   xmin = x_max-(x_max*0.18),
                                   xmax=x_max,
                                   ymin = y_min*(y_min*-0.09)
                                   )
  return(plot)
}


### Examples
p <- ggplot(iris, aes(y=Sepal.Length, x=Petal.Length)) + 
  geom_point(size=1, color="#3DD2DC") + 
  coord_cartesian(clip = "off") + 
  labs(title="Scatterplot of Sepal Length and Petal Length",
       y="Sepal Length",
       x="Penatal Length") + LiU_theme()
add_LiU_logo(p)

      