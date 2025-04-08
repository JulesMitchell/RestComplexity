# Code Example #
library(ggplot2)

# Set working dir where file is. File should contain electrode coordinates (x, y, z) in separate columns, electrode name and the value to plot (e.g. LZC) in it's own column.
setwd('')

# Load data
data <- read.csv()
ggplot(data,
       aes(x = x,
           y = y,
           fill = amplitude)) +
  geom_topo() + 
  scale_fill_distiller(palette = "RdBu") + 
  theme_void() + 
  coord_equal() + 
  labs(fill = expression(paste("Amplitude (", mu,"V)")))

  topoplot(data,
         palette = "viridis",
         highlights = c("Fp1, AF3, F3, FC1"),
         interp_limit = "head", 
         chan_marker = "point",
         quantity = "LZC",
         montage = NULL,
         highlights = NULL,
         scaling = 1)