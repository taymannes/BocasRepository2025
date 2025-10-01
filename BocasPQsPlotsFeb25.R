library(devtools)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(vegan)
library(reshape2)
library(patchwork)


setwd("/Users/taylormannes/Desktop/BOCAS DATA SEPT 2024")
agariciaporites<-read.csv("AgariciaPoritesCoverALL.csv")

#Some of these values are showing up as NA instead of zero, can fix
agariciaporites <- agariciaporites %>% mutate(across(c(Porites.Cover, Agaricia.Cover), ~ replace_na(.x, 0)))

#Adding cluster designations for colors
poritesT0T1cluster <- poritesavgfiltered %>%
  mutate(Cluster = case_when(
    Site %in% c("Pastores") ~ "1",
    Site %in% c("STRI") ~ "2",
    Site %in% c("Salt Creek","Coral Cay","Popa") ~ "3",
    Site %in% c("Cayo Roldan","Mainland") ~ "4",
    Site %in% c("Almirante","Caracol") ~ "5",
    Site %in% c("Hospital Point","Punta Juan") ~ "6"))

#Creating color scheme for sites. 6 clusters = 6 colors.
plot.col=c("violetred1","orangered","palegreen","deepskyblue1","lightslateblue","gold")

#plot

poritesplot<-ggplot(poritesT0T1cluster, aes(x = Time.Point, y = MeanPorites, group = Site, color = Cluster)) +
  geom_line() + 
  geom_point() +
  geom_text(data = poritesT0T1cluster %>% group_by(Site) %>% filter(Time.Point == "T7"), aes(label = Site), vjust = -0.5, size = 3, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +  # Minimal theme with larger text
  labs(title = "Porites Percent Cover",
       x = "Time Point",
       y = "Average Percent Cover") +  
  scale_color_manual(values = plot.col) 
poritesplot

ggsave("poritesplot.jpg", plot = poritesplot, scale = 1, dpi = 300)

#Plotting calcifiers
calcifiers<-read.csv("BocasPQsCalcifiersAll.csv")

#Creating average data frame
calcifiersavg <- calcifiers %>%
  group_by(Site, `Time.Point`) %>%
  summarize(MeanCalcifiers = mean(`Calcifiers.Cover`, na.rm = TRUE)) 


#Removing T8 & T9 from analysis
calcifiersavgfiltered <- calcifiersavg %>% filter(!Time.Point %in% c("T8", "T9"))

#Adding cluster designations for colors
calcifiersT0T1cluster <- calcifiersavgfiltered %>%
  mutate(Cluster = case_when(
    Site %in% c("Pastores") ~ "1",
    Site %in% c("STRI") ~ "2",
    Site %in% c("Salt Creek","Coral Cay","Popa") ~ "3",
    Site %in% c("Cayo Roldan","Mainland") ~ "4",
    Site %in% c("Almirante","Caracol") ~ "5",
    Site %in% c("Hospital Point","Punta Juan") ~ "6"))


calcifierplot<-ggplot(calcifiersT0T1cluster, aes(x = Time.Point, y = MeanCalcifiers, group = Site, color = Cluster)) +
  geom_line() + 
  geom_point() +
  geom_text(data = calcifiersT0T1cluster %>% group_by(Site) %>% filter(Time.Point == "T7"), aes(label = Site), vjust = -0.5, size = 3, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +  # Minimal theme with larger text
  labs(title = "Calcifiers Percent Cover",
       x = "Time Point",
       y = "Average Percent Cover") +  
  scale_color_manual(values = plot.col) 
calcifierplot

ggsave("calcifierplot.jpg", plot = calcifierplot, scale = 1, dpi = 300)

