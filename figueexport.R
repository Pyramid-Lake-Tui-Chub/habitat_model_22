## SCRIPT TO EXPORT PLOTS

#Load packages
library(gridExtra)

# Graph 1
setwd("C:\\Users\\A02377347\\Box\\School\\WATS 6800-Graduate Fish Ecology\\labs\\Lab 3")

png(filename = "gap_stat_4.png", units = "in", width = 8, height = 6, res=300)
fviz_gap_stat(gap_stat)
dev.off()

# Graph 2
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "lf_byMesh_23.png", units = "in", width = 6, height = 8, res=300)
lf_mesh
dev.off()

# Graph 3
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "Scuba10_july.png", units = "in", width = 6, height = 8, res=300)
grid.arrange(mat10_2, scub10, nrow=2, ncol=1, heights = c(1,2))
dev.off()