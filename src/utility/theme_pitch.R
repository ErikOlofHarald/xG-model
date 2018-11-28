theme_pitch <- function() {
  theme_minimal() %+replace%
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "#A3D1A3", colour = "#A3D1A3"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    )
}