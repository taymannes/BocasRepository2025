library(devtools)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(vegan)
library(reshape2)
library(patchwork)
library(ggrepel)
library(tidyr)


setwd("/Users/taylormannes/Desktop/BOCAS DATA")
agariciaporites<-read.csv("AgariciaPoritesCoverALL.csv")

#Some of these values are showing up as NA instead of zero, can fix
agariciaporites <- agariciaporites %>% mutate(across(c(Porites.Cover, Agaricia.Cover), ~ replace_na(.x, 0)))

#Creating average dataframe
apavgdata <- agariciaporites %>%
  group_by(Site, `Time.Point`) %>%   # Group by Site and Time Point
  summarize(
    Agaricia.Cover = mean(`Agaricia.Cover`, na.rm = TRUE),
    Porites.Cover = mean(`Porites.Cover`, na.rm = TRUE))%>%
    ungroup()

#Adding cluster initial designations for colors
apavgdata <- apavgdata %>%
  mutate(Cluster = case_when(
    Site %in% c("Pastores") ~ "1",
    Site %in% c("STRI") ~ "2",
    Site %in% c("Salt Creek","Coral Cay","Popa") ~ "3",
    Site %in% c("Cayo Roldan","Mainland") ~ "4",
    Site %in% c("Almirante","Caracol") ~ "5",
    Site %in% c("Hospital Point","Punta Juan") ~ "6"))

#Changing format of data frame for plotting 
apavgdata <- apavgdata %>%
  pivot_longer(cols = c(Porites.Cover, Agaricia.Cover), 
               names_to = "Coral.Type", 
               values_to = "Percent.Cover")

#plot
applot<-ggplot(apavgdata, aes(x = Time.Point, y = Percent.Cover, fill = Coral.Type)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +  #Bar plot with dodge to separate time points
  geom_smooth(aes(group = Coral.Type, color = Coral.Type), 
              method = "loess", se = FALSE, size = 1, linetype = "solid") +  #Trend lines
  scale_fill_manual(values = c("Porites.Cover" = "palegreen", "Agaricia.Cover" = "lightslateblue")) + 
  geom_smooth(aes(group =Coral.Type, color = Coral.Type), 
              method = "loess", se = FALSE, size = 1) +  #Add trend line with matching colors
  scale_color_manual(values = c("Porites.Cover" = "palegreen", "Agaricia.Cover" = "lightslateblue")) + 
  theme_minimal() +
  facet_wrap(~ Site, scales = "free_x")+
  labs(title = "Porites and Agaricia Cover Over time",
       x = "Time Point",
       y = "Mean Percent Cover",
       fill = "Coral Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  
        axis.title.x = element_text(face = "bold"),  
        axis.title.y = element_text(face = "bold"),  
        axis.text.y = element_text(face = "bold"),  
        strip.text = element_text(face = "bold"))  

applot

ggsave("agariciaporitesplot.jpg", plot = applot, scale = 2, dpi = 500)

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

###Plotting reef builders
reefbuilders<-read.csv("ReefBuildersALL.csv")

#Creating average data frame
reefbuildersavg <- reefbuilders %>%
  group_by(Site, `Time.Point`) %>%
  summarize(Reef.Builder.Cover = mean(`Reef.Builder.Cover`, na.rm = TRUE)) 

#Adding cluster designations for colors
reefbuildersavg <- reefbuildersavg %>%
  mutate(Cluster = case_when(
    Site %in% c("Pastores") ~ "1",
    Site %in% c("STRI") ~ "2",
    Site %in% c("Salt Creek","Coral Cay","Popa") ~ "3",
    Site %in% c("Cayo Roldan","Mainland") ~ "4",
    Site %in% c("Almirante","Caracol") ~ "5",
    Site %in% c("Hospital Point","Punta Juan") ~ "6"))

#Extracting the T9 for each site for site labels
corallabels <- reefbuildersavg %>%
  group_by(Site) %>%
  filter(Time.Point == max(Time.Point))

#Creating color scheme for sites. 6 clusters = 6 colors.
plot.col=c("violetred1","orangered","seagreen","royalblue3","mediumpurple3","tomato4")

reefbuilderplot<-ggplot(reefbuildersavg, aes(x = Time.Point, y = Reef.Builder.Cover, group = Site, color = Cluster)) +
  geom_line() + 
  geom_point() +
  geom_text_repel(data = corallabels, aes(label = Site), 
                  nudge_x = 0.5,  # Slight shift to avoid overlap
                  direction = "y",  # Adjust labels along the y-axis
                  hjust = 0, 
                  size = 5) + 
  theme_minimal(base_size = 14) +
  labs(title = "Reef Builders Percent Cover",
       x = "Time Point",
       y = "Mean Percent Cover") +  
  scale_color_manual(values = plot.col) 

reefbuilderplot



rbplot<-ggplot(reefbuildersavg, aes(x = Time.Point, y = Reef.Builder.Cover, fill = as.factor(Cluster))) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  #Bar plot with dodge to separate time points
  geom_smooth(aes(group = Cluster, color = as.factor(Cluster)), 
              method = "loess", se = FALSE, size = 1, linetype = "solid") +  #Trend lines
  scale_fill_manual(values = plot.col) +  #Custom colors
  scale_color_manual(values = plot.col) + 
  facet_wrap(~Site, scales = "free_x")+ 
  theme_minimal() +
  labs(title = "Reef Builder Percent Cover Over time",
       x = "Site and Time Point",
       y = "Average Reef Builder Cover (%)",
       fill = "Cluster",
       color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold"),  
    axis.text.y = element_text(face = "bold"), 
    strip.text = element_text(face = "bold"))  

rbplot
ggsave("reefbuilderplot.jpg", plot = rbplot, scale = 2, dpi = 500)

##Reef builders and fleshy organism plot
fleshyreefbuilder<-read.csv("fleshvsreefbuilderALL.csv")


#Creating average dataframe
Fbfrb <- fleshyreefbuilder %>%
  group_by(Site, `Time.Point`) %>%   # Group by Site and Time Point
  summarize(
    MeanReefBuilders = mean(`Reef.Builder.Cover`, na.rm = TRUE),
    MeanFleshy = mean(`Fleshy.Organism.Cover`, na.rm = TRUE),
    MeanFireCoral = mean(`Fire.Coral`, na.rm = TRUE))%>%
  ungroup()

#Adding cluster initial designations for colors
frb <- frb %>%
  mutate(Cluster = case_when(
    Site %in% c("Pastores") ~ "1",
    Site %in% c("STRI") ~ "2",
    Site %in% c("Salt Creek","Coral Cay","Popa") ~ "3",
    Site %in% c("Cayo Roldan","Mainland") ~ "4",
    Site %in% c("Almirante","Caracol") ~ "5",
    Site %in% c("Hospital Point","Punta Juan") ~ "6"))

#Changing data structure to "long" for plotting
frb <- frb %>%
  pivot_longer(cols = c(MeanReefBuilders, MeanFleshy, MeanFireCoral), 
               names_to = "Organism.Type", 
               values_to = "Percent.Cover")

#plot
frbplot<-ggplot(frb, aes(x = Time.Point, y = Percent.Cover, fill = Organism.Type)) + 
  geom_col(
    data = subset(frb, Organism.Type %in% c("MeanReefBuilders", "MeanFleshy")),
    aes(y = Percent.Cover, fill = Organism.Type),
    position = "stack") +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) + 
  scale_fill_manual(values = c("MeanFleshy" = "lightgreen", "MeanReefBuilders" = "orange")) + # Custom colors
  #geom_smooth(aes(group =Organism.Type, color = Organism.Type), 
              #method = "loess", se = FALSE, size = 1) +  #Add trend line for each group with matching colors
  geom_smooth(data = frb %>% filter(Organism.Type == "MeanFireCoral"),
    aes(x = as.numeric(factor(Time.Point)), y = Percent.Cover, color = "FireCoral"),
    method = "loess",
    se = FALSE,
    size = 1.5) +
  scale_fill_manual(values = c("MeanFleshy" = "lightgreen", "MeanReefBuilders" = "orange")) +
  scale_color_manual(values = c("FireCoral" = "darkgray"), name = " ")+
  theme_minimal() +
  facet_wrap(~ Site, scales = "free_x")+
  labs(title = "Reef Builders and Fleshy Organisms Over Time",
    x = "Time Point",
       y = "Mean Percent Cover",
       fill = "Organism Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Bold x-axis labels
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),  
        strip.text = element_text(face = "bold"))  

frbplot

ggsave("fleshyorganismsreefbuilderstack.jpg", plot = frbplot, scale = 2, dpi = 500)

##Plotting branching Porites per site


porites<-read.csv("BranchingPoritesALL.csv")
poritesavgdata <- porites %>%
  group_by(Site, `Time.Point`) %>%   
  summarize(
    Branching.Porites.Cover = mean(`Branching.Porites.Cover`, na.rm = TRUE))%>%
  ungroup()

branchingporitesplot<-ggplot(poritesavgdata, aes(x = Time.Point, y = Branching.Porites.Cover, group = Site)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ Site)+
  theme_minimal(base_size = 14) +
  labs(title = "Branching Porites Percent Cover",
       x = "Time Point",
       y = "Mean Percent Cover") +  
  scale_color_manual(values = plot.col) 
ggsave("branchingporitesplot.jpg", plot = branchingporitesplot, scale = 2, dpi = 500)

#Plotting branching porites and fleshy organisms w/ fire coral overlay.

#combining fleshy organisms and porites plots
branchporfrb <- read.csv("fleshybranchingporites.csv")

#creating average data frame
avgporflesh <- branchporfrb %>%
  group_by(Site, `Time.Point`) %>%   # Group by Site and Time Point
  summarize(
    MeanBranchingPorites = mean(`Branching.Porites.Cover`, na.rm = TRUE),
    MeanFleshy = mean(`Fleshy.Organism.Cover`, na.rm = TRUE),
    MeanFireCoral = mean(`Fire.Coral`, na.rm = TRUE))%>%
  ungroup()

#Changing data structure to "long" for plotting
avgporflesh <- avgporflesh %>%
  pivot_longer(cols = c(MeanBranchingPorites, MeanFleshy, MeanFireCoral), 
               names_to = "Organism.Type", 
               values_to = "Percent.Cover")

#Plot
fleshporplot<-ggplot(avgporflesh, aes(x = Time.Point, y = Percent.Cover, fill = Organism.Type)) + 
  geom_col(
    data = subset(avgporflesh, Organism.Type %in% c("MeanBranchingPorites", "MeanFleshy")),
    aes(y = Percent.Cover, fill = Organism.Type),
    position = "stack") +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) + 
  scale_fill_manual(values = c("MeanFleshy" = "lightgreen", "MeanBranchingPorites" = "purple")) + # Custom colors
  #geom_smooth(aes(group =Organism.Type, color = Organism.Type), 
  #method = "loess", se = FALSE, size = 1) +  #Add trend line for each group with matching colors
  #geom_smooth(data = avgporflesh %>% filter(Organism.Type == "MeanFireCoral"),
              #aes(x = as.numeric(factor(Time.Point)), y = Percent.Cover, color = "FireCoral"),
              #method = "loess",
              #se = FALSE,
              #size = 1.5) +
  scale_fill_manual(values = c("MeanFleshy" = "lightgreen", "MeanBranchingPorites" = "purple")) +
  #scale_color_manual(values = c("FireCoral" = "orange"), name = " ")+
  theme_minimal() +
  facet_wrap(~ Site, scales = "free_x")+
  labs(title = "Branching Porites and Fleshy Organisms Over Time",
       x = "Time Point",
       y = "Mean Percent Cover",
       fill = "Organism Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Bold x-axis labels
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),  
        strip.text = element_text(face = "bold"))  
fleshporplot
ggsave("branchingporitesfleshyorganisms.jpg", plot = fleshporplot, scale = 2, dpi = 500)
