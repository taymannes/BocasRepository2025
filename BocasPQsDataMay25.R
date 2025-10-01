#Bocas del Toro benthic community cover analysis May 2025
library(devtools)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(vegan)
library(reshape2)
library(patchwork)
library(ecodist)
library(tidyr)

#Setting up the data frame, making sure that NA values added by accident are omitted
setwd("/Users/taylormannes/Desktop/BOCAS DATA")
bocasdata<-read.csv("BocasPQsALLDATAJan2025.csv")
bocasdata=na.omit(bocasdata)

#Making sure each benthic community category is treated as a number
bocasdata$Live.Hard.Coral <- as.numeric(bocasdata$Live.Hard.Coral)
bocasdata$Dead.Coral <- as.numeric(bocasdata$Dead.Coral)
bocasdata$Soft.Coral..Fire.Coral <- as.numeric(bocasdata$Soft.Coral..Fire.Coral)
bocasdata$Zoanthid <- as.numeric(bocasdata$Zoanthid)
bocasdata$Other.Invertebrates <- as.numeric(bocasdata$Other.Invertebrates)
bocasdata$Sponge <- as.numeric(bocasdata$Sponge)
bocasdata$CCA <- as.numeric(bocasdata$CCA)
bocasdata$Other.Algae <- as.numeric(bocasdata$Other.Algae)
bocasdata$Substrate..Rubble..sand. <- as.numeric(bocasdata$Substrate..Rubble..sand.)

#Creating dataframe with average values
bocasavgdata <- bocasdata %>%
  group_by(Site, `Time.Point`) %>%   # Group by Site and Time Point
  summarize(
    LiveHardCoral = mean(`Live.Hard.Coral`, na.rm = TRUE),
    SoftorFireCoral = mean(`Soft.Coral..Fire.Coral`, na.rm = TRUE),
    DeadCoral = mean(`Dead.Coral`, na.rm = TRUE),
    Zoanthid = mean(`Zoanthid`, na.rm = TRUE),
    Sponge = mean(`Sponge`, na.rm = TRUE),
    OtherInvertebrates = mean(`Other.Invertebrates`, na.rm = TRUE),
    OtherAlgae = mean(`Other.Algae`, na.rm = TRUE),
    Substrate = mean(`Substrate..Rubble..sand.`, na.rm = TRUE)
  ) %>%
  ungroup()

#Adding Cluster Classification for T0, T1 in average dataset
bocasavgdata <- bocasavgdata %>%
  mutate(Cluster = case_when(
    Site %in% c("Pastores") ~ "1",
    Site %in% c("STRI") ~ "2",
    Site %in% c("Salt Creek","Coral Cay","Popa") ~ "3",
    Site %in% c("Cayo Roldan","Mainland") ~ "4",
    Site %in% c("Almirante","Caracol") ~ "5",
    Site %in% c("Hospital Point","Punta Juan") ~ "6"))


#Trying to run all site NMDS
Sites<-as.vector(bocasavgdata$Site)
Cluster<-as.vector(bocasavgdata$Cluster)
NMDSmatrix=as.matrix(bocasavgdata[, -c(1, 2,11)])
bocasNMDS=metaMDS(NMDSmatrix, k=2, trymax=100)

##Using the scores function from vegan to extract the site scores and convert to a data.frame
bocas.scores <- as.data.frame(vegan::scores(bocasNMDS)$site)
##Create column of time points
bocas.scores$TimePoint <- bocasavgdata$Time.Point
##Add the site variable
bocas.scores$Site <- Sites 
####Can save as a data frame if you want
#write.csv(bocas.scores, "bocasNMDSscores.csv", row.names = FALSE)


###Trying to get the point coordinates for euclidean distance calculations

#Extract species scores for relevant time points
species_time_data <- bocasavgdata %>%
  select(Site, Time.Point, where(is.numeric)) %>%  # Keep only numeric columns
  pivot_longer(cols = -c(Site, Time.Point), names_to = "Category", values_to = "Abundance")


#Filter NMDS coordinates for initial (T0/T1) and final (T9) time points
initialcoord <- bocas.scores %>% filter(TimePoint %in% c("T0", "T1"))
finalcoord <- bocas.scores %>% filter(TimePoint == "T9")

#Merge initial and final NMDS coordinates by Site
nmdscoordinates<- initialcoord %>%
  inner_join(finalcoord, by = "Site", suffix = c("_initial", "_final"))


##Calculating euclidean distances 
nmdscoordinates <- nmdscoordinates %>%
  mutate(EucDist = sqrt((NMDS1_final - NMDS1_initial)^2 + (NMDS2_final - NMDS2_initial)^2))



#Creating color scheme for sites. 6 clusters = 6 colors.
plot.col=c("violetred1","orangered","palegreen","deepskyblue1","lightslateblue","gold")

##Using the scores function from vegan to extract the species scores and convert to a data.frame
benthic.scores <- as.data.frame(vegan::scores(bocasNMDS, "species"))
# create a column of benthic categories, from the rownames of benthic.scores
benthic.scores$Category <- rownames(benthic.scores) 


NMDS.initial.clusters= ggplot() + 
  theme_classic(base_size=14) +
  ggtitle("T0,T1 Cluster Trajectory") +
  geom_path(data=bocas.scores,aes(x=NMDS1,y=NMDS2,group=Site,color=Cluster), linewidth=1) + 
  scale_color_manual(values = plot.col)+ #Changing colors to predefined color palette
  geom_text(data=bocas.scores,aes(x=NMDS1,y=NMDS2,label=TimePoint),size=3) + #Add time point labels
  geom_text(
    data = bocas.scores %>% 
      group_by(Site) %>% 
      slice(1) %>%  # Select the first row for each site
      mutate(NMDS2 = NMDS2 - 0.02),  # Shift labels slightly below
    aes(x = NMDS1, y = NMDS2, label = Site), 
    size = 4, 
    fontface = "bold", 
    color = "black"
  ) +
  geom_text(data=benthic.scores,aes(x=NMDS1,y=NMDS2,label=Category), size=3) +  # add the species labels
  geom_text(data = nmdscoordinates, aes(x = NMDS1_final, y = NMDS2_final, label = round(EucDist, 3)), 
            vjust = -1.5, size = 3, color = "black", fontface = "italic", na.rm = TRUE) +  
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  annotate(geom="label", x=-0.6, y=-0.5, label="Stress=0.09", size=4.5)

NMDS.initial.clusters

ggsave("NMDS.initial.clusters.jpg", plot = NMDS.initial.clusters, scale = 2, dpi = 300)

#####Adding Cluster Classification for T9 in main dataset
bocasdataT9cluster <- bocasdata %>%
  mutate(Cluster = case_when(
    Site %in% c("Popa","Coral Cay","Salt Creek") ~ "2",
    Site %in% c("Almirante","STRI","Cayo Roldan","Mainland","Pastores","Punta Juan","Caracol","Hospital Point") ~ "1"))

#Adding Cluster Classification for T0, T1 in average dataset
bocasavgdataT9cluster <- bocasavgdata %>%
  mutate(Cluster = case_when(
    Site %in% c("Popa","Coral Cay","Salt Creek") ~ "2",
    Site %in% c("Almirante","STRI","Cayo Roldan","Mainland","Pastores","Punta Juan","Caracol","Hospital Point") ~ "1"))


#Trying to run all site NMDS
SitesT9<-as.vector(bocasavgdataT9cluster$Site)
ClusterT9<-as.vector(bocasavgdataT9cluster$Cluster)
NMDSmatrixT9=as.matrix(bocasavgdataT9cluster[, -c(1, 2,11)])
bocasNMDST9=metaMDS(NMDSmatrixT9, k=2, trymax=100)

##Using the scores function from vegan to extract the site scores and convert to a data.frame
bocas.scoresT9 <- as.data.frame(vegan::scores(bocasNMDST9)$sites)
##Create column of time points
bocas.scoresT9$TimePoint <- bocasavgdataT9cluster$Time.Point
##Add the site variable
bocas.scoresT9$Site <- SitesT9
head(bocas.scoresT9)
##Using the scores function from vegan to extract the species scores and convert to a data.frame
benthic.scoresT9 <- as.data.frame(vegan::scores(bocasNMDST9, "species"))
# create a column of benthic categories, from the rownames of benthic.scores
benthic.scoresT9$Category <- rownames(benthic.scoresT9) 

#Creating color scheme for sites. 2 clusters = 2 colors.
plot.colT9=c("violetred1","deepskyblue1")


NMDS.T9.clusters= ggplot() + 
  theme_classic(base_size=14) +
  ggtitle("T9 Cluster Trajectory") +
  geom_path(data=bocas.scoresT9,aes(x=NMDS1,y=NMDS2,group=Site,color=ClusterT9), linewidth=1) + 
  scale_color_manual(values = plot.colT9)+ #Changing colors to predefined color palette
  geom_text(data=bocas.scoresT9,aes(x=NMDS1,y=NMDS2,label=TimePoint),size=3) + #Add time point labels
  geom_text(
    data = bocas.scoresT9 %>% 
      group_by(Site) %>% 
      slice(1) %>%  # Select the first row for each site
      mutate(NMDS2 = NMDS2 - 0.02),  # Shift labels slightly below
    aes(x = NMDS1, y = NMDS2, label = Site), 
    size = 4, 
    fontface = "bold", 
    color = "black"
  ) +
  geom_text(data=benthic.scoresT9,aes(x=NMDS1,y=NMDS2,label=Category), size=3) +  # add the species labels
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black")) +
  annotate(geom="label", x=-0.6, y=-0.5, label="Stress=0.09", size=4.5)

NMDS.T9.clusters

ggsave("NMDS.T9.clusters.jpg", plot = NMDS.T9.clusters, scale = 1, dpi = 300)

trajectoryplots<-NMDS.initial.clusters+NMDS.T9.clusters
ggsave(
  filename = "trajectory cluster plots.png", plot = trajectoryplots, width = 10, height = 5, dpi = 300)


##Benthic community change over time, coral
df_coral <- bocasavgdata %>% 
  select(Site, `Time.Point`, `LiveHardCoral`)

coralplot<-ggplot(df_coral, aes(x = `Time.Point`, y = `LiveHardCoral`, group = Site, color = ClusterT9)) +
  ggtitle("Hard Coral Cover")+
  geom_line() + 
  geom_point() + 
  facet_wrap(~ Site) +  # Creates a separate plot for each site
  labs(x = "Time Point", y = "Average Percent Cover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
coralplot

#Sponge plot
df_sponge <- bocasavgdata %>% 
  select(Site, `Time.Point`, `Sponge`)

spongeplot<-ggplot(df_sponge, aes(x = `Time.Point`, y = `Sponge`, group = Site, color = ClusterT9)) +
  ggtitle("Sponge Cover")+
  geom_line() + 
  geom_point() + 
  facet_wrap(~ Site) +  # Creates a separate plot for each site
  labs(x = "Time Point", y = "Average Percent Cover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
spongeplot

#Algae
df_algae <- bocasavgdata %>% 
  select(Site, `Time.Point`, `OtherAlgae`)

algaeplot<-ggplot(df_algae, aes(x = `Time.Point`, y = `OtherAlgae`, group = Site, color = ClusterT9)) +
  ggtitle("Macroalgae Cover")+
  geom_line() + 
  geom_point() + 
  facet_wrap(~ Site) +  # Creates a separate plot for each site
  labs(x = "Time Point", y = "Average Percent Cover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
algaeplot

#zoanthid
df_zoanthid <- bocasavgdata %>% 
  select(Site, `Time.Point`, `Zoanthid`)

zoanthidplot<-ggplot(df_zoanthid, aes(x = `Time.Point`, y = `Zoanthid`, group = Site, color = ClusterT9)) +
  ggtitle("Zoanthid Cover")+
  geom_line() + 
  geom_point() + 
  facet_wrap(~ Site) +  # Creates a separate plot for each site
  labs(x = "Time Point", y = "Average Percent Cover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
zoanthidplot

benthicgroupplot<-coralplot+algaeplot+spongeplot+zoanthidplot
ggsave(
  filename = "benthicgroupplot.png", plot = benthicgroupplot, width = 10, height = 5, dpi = 300)


##Euclidean Distances vs. coral cover
eucdist<-read.csv("EuclideanDistBocas.csv")


#Adding coral %cover from larger dataset, but excluding initial time points (T0,T1) b/c they do not have a Euclidean dist. calculation 
bocasavgdata<- bocasavgdata %>%
  filter(!Time.Point %in% c("T0", "T1"))


#adding coral & algae data to Euclidean distance data set
eucdist$LiveCoralCover<-bocasavgdata$LiveHardCoral
eucdist$Algae<-bocasavgdata$OtherAlgae

#colors for plotting, 11 colors for 11 sites
euccolors<-c("mediumpurple1","palegreen3","dodgerblue","deeppink","salmon2","gold1","darkblue","burlywood4","purple4","darkolivegreen","palevioletred")
#plotting coral
ggplot(eucdist, aes(x = LiveCoralCover, y = EucDist, color = Site, group = Site)) +
  geom_line() +  # Connect points over time for each site
  geom_point() +  # Add points at each time
  facet_wrap(~ Site) +  # Create separate plots for each site
  scale_color_manual(values = euccolors) +  # Custom colors for sites
  labs(x = "Coral % Cover", y = "Euclidean Distance", color = "Site") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_text_repel(aes(label = TimePoint)) # Add time point labels

ggsave("euclidean_vs_coral_plot.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

#plotting algae
ggplot(eucdist, aes(x = Algae, y = EucDist, color = Site, group = Site)) +
  geom_line() +  # Connect points over time for each site
  geom_point() +  # Add points at each time
  facet_wrap(~ Site) +  # Create separate plots for each site
  scale_color_manual(values = euccolors) +  # Custom colors for sites
  labs(x = "Macroalgae % Cover", y = "Euclidean Distance", color = "Site") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_text_repel(aes(label = TimePoint)) # Add time point labels

ggsave("euclidean_vs_algae_plot.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

##Plotting one site NMDS trajectory
bocasdata<-read.csv("BocasPQsALLDATAJan2025.csv")
bocasdata=na.omit(bocasdata)

hospitalpoint<- bocasdata %>%
  filter(Site == "Hospital Point")


#Ordering data by time point
hospitalpoint$Time.Point <- factor(hospitalpoint$Time.Point, levels = unique(hospitalpoint$Time))

#Creating an ID for each plot per time point
hospitalpoint$Replicate_ID <- as.factor(paste0("R", ave(1:nrow(hospitalpoint), hospitalpoint$Time.Point, FUN = seq_along)))

#Run NMDS
set.seed(42)
HPnmds <- metaMDS(hospitalpoint[, -c(1, 2,11,12)], distance = "euclidean", k = 2, trymax = 100)

# Extract NMDS coordinates
nmds_points <- as.data.frame(HPnmds$points)
nmds_points$Time.Point <- hospitalpoint$Time.Point
nmds_points$Replicate_ID <- hospitalpoint$Replicate_ID

#Calculate Euclidean distances between consecutive time points
nmds_points <- nmds_points %>%
  arrange(Time.Point) %>%
  mutate(Distance = sqrt((lag(MDS1) - MDS1)^2 + (lag(MDS2) - MDS2)^2))




#Adding "categories" of species to data frame to include labels for plotting
HPspptime <- hospitalpoint %>%
  select(Time.Point, where(is.numeric)) %>%  # Keep only numeric columns
  pivot_longer(cols = -c(Time.Point), names_to = "Category", values_to = "Abundance")

#Creating more labels
label_data <- nmds_points %>%
  mutate(Time.Numeric = as.numeric(gsub("T", "", as.character(Time.Point)))) %>%
  group_by(Replicate_ID) %>%
  filter(Time.Numeric == max(Time.Numeric)) %>%
  ungroup() %>%
  as.data.frame()
#Creating color scheme for replicates
plot.col=c("mediumpurple1","palegreen3","dodgerblue","deeppink","salmon2","gold1","darkblue","burlywood4","purple4","darkolivegreen","palevioletred")


HP.NMDS <- ggplot() + 
  theme_classic(base_size = 14) +
  ggtitle("Hospital Point Cluster Trajectory") +
  geom_path(
    data = nmds_points,
    aes(x = MDS1, y = MDS2, color = Replicate_ID),
    linewidth = 1) + 
  geom_text(
    data = nmds_points,
    aes(x = MDS1, y = MDS2, label = Time.Point),
    size = 3) + 
  geom_text(
    data = label_data,
    aes(x = MDS1, y = MDS2, label = Replicate_ID),
    size = 4,
    fontface = "bold",
    color = "black",
    nudge_x = -0.03 ) +
  scale_color_manual(values = plot.col) +
  theme(plot.title = element_text(hjust = 0.5),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")) +
  annotate(geom = "label", x = -0.6, y = -0.5, label = "Stress = 0.09", size = 4.5)

HP.NMDS
ggsave("HospitalPointNMDStrajectory.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
