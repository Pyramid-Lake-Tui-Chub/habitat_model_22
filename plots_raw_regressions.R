#### INSTALL PACKAGES ----
library(tidyverse)
library(tidyr)
library(plotrix)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(dplyr)
theme_set(theme_cowplot())

#### PLOTS ----

### HYDROACOUSTICS ----

# substrate

clay <- ggplot(master, aes(x=perc_clay, y=log(matureallCpue + 0.08))) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  ylim(-3,2) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "% Clay (<.0005 mm)", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)")
clay 

sand <- ggplot(master, aes(perc_sand, y=log(matureallCpue + 0.08))) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  ylim(-3,2) +
  scale_color_brewer(palette="Dark2") +
  labs( x= "% Sand (0.05-2 mm)", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)")
sand

silt<- ggplot(master, aes(x=perc_silt, y=log(matureallCpue + 0.08))) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  ylim(-3,2) +
  scale_color_brewer(palette="Dark2") +
  labs( x= "% Silt (.0005 - 0.05 mm)", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)")
silt

rock <- ggplot(master, aes(x=perc_rock, y=log(matureallCpue + 0.08))) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  ylim(-3, 2) +
  scale_color_brewer(palette="Dark2") +
  labs( x= "% Rock (>2 mm)", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)")
rock

grid.arrange(clay, sand, silt, rock, nrow=2, ncol=2)

# facet wrap version of substrate
hydro_fish_1 <- subset(hydro_fish, avg_vegheight != "NA" & month == 6)
sub_facet_dat <- melt(hydro_fish_1[, c(1:3, 8:11, 23)], id.vars = c("site", "month", "depth_bin", "matureallCpue"))
sub_names <- c("perc_clay" = "Clay (<.0005 mm)","perc_sand"="Sand (0.05-2 mm)","perc_silt"= "Silt (.0005 - 0.05 mm)","perc_rock"="Rock (>2 mm)")

sub_facet <- ggplot(data= sub_facet_dat, aes(x=value, y=log(matureallCpue + 0.08))) +
  facet_wrap(~variable, labeller=as_labeller(sub_names)) +
  geom_point() +
  geom_smooth(method = "lm", orientation = "x", color = "turquoise4", alpha=0.6) +
  scale_x_continuous(position="bottom")+
  theme(plot.title = element_text(hjust=0.5))+
  labs( x= "Proportion of Site Substrate Composition", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)") 
sub_facet


# vegetation

veg_height <- ggplot(sub_june, aes(x=avg_vegheight, y= log(matureallCpue + 0.08))) + 
  geom_point()+
  geom_smooth(method=loess, color="turquoise4") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Average Vegetation Height (m)", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)")
veg_height

veg_cover <- ggplot(master, aes(x=avg_vegcover, y=matureallCpue)) + 
  geom_point()+
  #scale_y_continuous(trans="log2") +
  geom_smooth(method=lm, color="turquoise4") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Average Vegetation Cover (%)", y= "CPUE of Fecund Fish (fish/hour)")
veg_cover

### DEPTH BIN ----
depth_bin <- ggplot(data = subset(master, depth_bin != 0), aes(x=as.factor(depth_bin), y=log(matureallCpue + 0.08))) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  scale_x_discrete(labels = c("1" = "0-10", "2"="10-20","3"="20-30")) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Depth (m)", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)")
depth_bin

### MONTH ----
month <- ggplot(master, aes(x=as.factor(month), y=matureallCpue)) + 
  geom_boxplot() +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 90),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Month", y= "CPUE of Fecund Fish (fish/hour)")
month

### DATE ----
date <- ggplot(master, aes(x=date_time_set, y=matureallCpue)) + 
  geom_point()+
  geom_jitter() +
  geom_smooth(method=loess, color = "turquoise4", se=F)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 90),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Date", y= "CPUE of Fecund Fish (fish/hour)") +
  facet_wrap(~depth_bin)
date

### NET TYPE ----
net_type <- ggplot(master, aes(x=net_type, y=matureallCpue)) + 
  geom_boxplot()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 90),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Net Type", y= "CPUE of Fecund Fish (fish/hour)")
net_type

### SCUBA ----

# substrate
bedrock <- ggplot(scuba_fish, aes(x=bedrock, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% bedrock", y= "CPUE of Fecund Fish (fish/hour)")
bedrock

boulder <- ggplot(scuba_fish, aes(x=boulder, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% boulder", y= "CPUE of Fecund Fish (fish/hour)")
boulder

cobble <- ggplot(scuba_fish, aes(x=cobble, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% cobble", y= "CPUE of Fecund Fish (fish/hour)")
cobble

gravel <- ggplot(scuba_fish, aes(x=gravel, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% gravel", y= "CPUE of Fecund Fish (fish/hour)")
gravel

smallgravel <- ggplot(scuba_fish, aes(x=smallgravel, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% smallgravel", y= "CPUE of Fecund Fish (fish/hour)")
smallgravel

sand <- ggplot(scuba_fish, aes(x=sand, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% sand", y= "CPUE of Fecund Fish (fish/hour)")
sand

clay_fines <- ggplot(scuba_fish, aes(x=clay_fines, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% Silt", y= "CPUE of Fecund Fish (fish/hour)")
clay_fines

hard_clay <- ggplot(scuba_fish, aes(x=hard_clay, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% Clay", y= "CPUE of Fecund Fish (fish/hour)")
hard_clay

perc_rock <- ggplot(scuba_fish, aes(x=perc_rock, y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "% Rock", y= "log(CPUE of Fecund Fish +1) (fish/hour)")
perc_rock

# vegetation/cladophora (algae)
veg_low <- ggplot(data=scuba_fish, aes(x= veg_low , y= lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "<.75 mm vegetation", y= "CPUE of Fecund Fish (fish/hour)")
veg_low

veg_mid <- ggplot(data=scuba_fish, aes(x= veg_mid , y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= ".75-100 mm vegetation", y= "CPUE of Fecund Fish (fish/hour)")
veg_mid

veg_high <- ggplot(data=scuba_fish, aes(x= veg_high , y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "100 mm+ vegetation", y= "CPUE of Fecund Fish (fish/hour)")
veg_high

clad <- ggplot(data=scuba_fish, aes(x= clad , y=lnCPUE)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Cladophora", y= "CPUE of Fecund Fish (fish/hour)")
clad

### LIMNOLOGY ----

# clarity
clarity <- ggplot(data=master, aes(x= clarity , y= matureallCpue)) + 
  geom_point()+
  #geom_smooth(method=loess) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Water Transparency (m)", y= "CPUE of Fecund Fish (fish/hour)")
clarity

# conductivity
conduc <- ggplot(master, aes(x= conductivity , y= matureallCpue)) + 
  geom_point()+
  #geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Conductivity (mg/L)", y= "CPUE of Fecund Fish (fish/hour)")
conduc

# temperature-from Gary's data
temp <- ggplot(sub_june, aes(x= temp_avg , y= log(matureallCpue + 0.08))) + 
  geom_point()+
  geom_smooth(method=lm, color="turquoise4") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "Average Temperature (C)", y= "log(CPUE of Fecund Fish (fish/hour) + 0.08)")
temp
