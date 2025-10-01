setwd("C:/Users/adikh/Desktop")
library(devtools)
library(ggord)
library(ggplot2)
library(RColorBrewer)
library(vegan)
library(reshape2)

data=read.csv(file="Palmyra PQs percent cover by quad and group.csv", header=TRUE, sep=",")
data=na.omit(data)
data=aggregate(Percent.cover~Date+Site+Benthic.group, data, mean )

data_2=dcast(data, Date + Site ~ Benthic.group, value.var = "Percent.cover")
data_FR= data_2[grep("FR", data_2$Site),]
data_RT= data_2[grep("RT", data_2$Site),]


#FR
FR.sites <- as.vector(data_FR$Site)
FR.mds=metaMDS(data_FR[,c(3:7)], distance = "bray", k=2, autotransform = F)
#FR.scores <- as.data.frame(scores(FR.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
FR.scores <- as.data.frame(vegan::scores(FR.mds)$sites)
FR.scores$Date <- data_FR$Date  # create a column of time points
FR.scores$Site <- FR.sites  #  add the site variable
head(FR.scores)  #look at the data
species.scores <- as.data.frame(scores(FR.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$Category <- rownames(species.scores)  # create a column of benthic categories, from the rownames of species.scores
head(species.scores)  #look at the data

FR_NMDS.over.time= ggplot() + 
  theme_classic(base_size=14) +
  ggtitle("Trajectory of Benthic Community Composition at FR") +
  geom_path(data=FR.scores,aes(x=NMDS1,y=NMDS2,colour=Site), size=1) + # add the point markers
  geom_text(data=FR.scores,aes(x=NMDS1,y=NMDS2,label=Date),size=3) + # add the date labels 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=Category), size=5) +  # add the species labels
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  annotate(geom="label", x=-0.6, y=-0.5, label="Stress=0.09", size=4.5)
ggsave("NMDS over time for FR sites.jpg", plot = FR_NMDS.over.time, scale = 1, dpi = 300)


#RT
RT.sites <- as.vector(data_RT$Site)
RT.mds=metaMDS(data_RT[,c(3:7)], distance = "bray", k=2, autotransform = F)
RT.scores <- as.data.frame(vegan::scores(RT.mds)$sites)
RT.scores$Date <- data_RT$Date  # create a column of time points
RT.scores$Site <- RT.sites  #  add the site variable
head(RT.scores)  #look at the data
species.scores <- as.data.frame(scores(RT.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$Category <- rownames(species.scores)  # create a column of benthic categories, from the rownames of species.scores
head(species.scores)  #look at the data

RT_NMDS.over.time= ggplot() + 
  theme_classic(base_size=14) +
  ggtitle("Trajectory of Benthic Community Composition at RT") +
  geom_path(data=RT.scores,aes(x=NMDS1,y=NMDS2,colour=Site), size=1) + # add the point markers
  geom_text(data=RT.scores,aes(x=NMDS1,y=NMDS2,label=Date),size=3) + # add the date labels 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=Category), size=5) +  # add the species labels
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  annotate(geom="label", x=-0.75, y=-0.3, label="Stress=0.07", size=4.5)
ggsave("NMDS over time for RT sites.jpg", plot = RT_NMDS.over.time, scale = 1, dpi = 300)

require(gridExtra)
grid.arrange(FR_NMDS.over.time, RT_NMDS.over.time, nrow=1)
