# Set EG theme to use in all charts
theme_eg <- function(base_size = 10, base_family = "Helvetica")
{
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.position  = "bottom",
      legend.title     = element_text(colour="#111111", size = 8, face = "bold"),
      legend.key       = element_blank(),
      plot.title       = element_text(face="bold", colour="#111111", size=15, hjust=0.5, vjust=2),
      axis.title.x     = element_text(face="bold", colour="#111111", size=10),
      axis.title.y     = element_text(face="bold", colour="#111111", size=10),
      axis.text.x      = element_text(colour="#333333", size=8),
      axis.text.y      = element_text(colour="#333333", size=8),
      plot.background  = element_rect(colour = NA),
      panel.border     = element_blank(),
      panel.grid.major = element_line(colour = "black", size = 0.05),
      panel.grid.minor = element_line(colour = "black", size = 0.05),
      panel.margin     = unit(0.0, "lines")
    ) 
}

color_permit <- "#16A9E3"
color_completion <- "#7FC06B"
color_spud <- "#FF7F0E"
color_frac <- "#E62D34"
color_rig <- "#ff7f0e"

color_scale <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}