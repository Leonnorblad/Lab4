#' Graphics for Linköping University
#' 
#' A ggplot2 theme for LiU. Recommend to render plot by cairo_pdf device in 4 x 6 dimensions.
#' 
#' @import ggplot2
#' 
#' @examples
#'# data(iris)
#'#  ggplot(iris, aes(y=Sepal.Length, x=Petal.Length)) + 
#'#  geom_point(size=1) + 
#'#  labs(title="Scatterplot of Sepal Length and Petal Length",
#'#       y="Sepal Length",
#'#       x="Penatal Length") +
#'#  LiU_theme()
#' 
#' 
#' @export


LiU_theme <- function(){
  theme(# Title
        plot.title = element_text(hjust=0.5,        # Center the title
                                  face="bold",      # Bold text
                                  size=14,          # Change the size
                                  color="#3DD2DC"), # LiU blue colour
        # Axis title
        axis.title = element_text(face="bold",      # Bold text
                                  size=12,          # Change the size
                                  color="#3DD2DC"), # LiU blue colour
        # Axis text
        axis.text = element_text(size=10),               # Change the size
        panel.border = element_rect(fill = NA,          # No border colour  
                                    colour = "grey20"), # Gray colour for the border
        panel.grid = element_line(colour = "grey85"),   # Gray color for the grid
        panel.background = element_rect(fill = "white", # White background
                                        colour = NA)    # No colour
  )
}
