#hello? 
#Bocas del Toro benthic community cover analysis Sept. 2024
library(devtools)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(vegan)
library(reshape2)

#Setting up the data frame, making sure that NA values added by accident are omitted
setwd("/Users/taylormannes/Desktop/BOCAS DATA SEPT 2024")
bocasdata<-read.csv("BdTCompiled.csv")
bocasdata=na.omit(bocasdata)

#Making sure each benthic community category is treated as a number
bocasdata$Live.Hard.Coral <- as.numeric(bocasdata$Live.Hard.Coral)
bocasdata$Dead.Coral <- as.numeric(bocasdata$Dead.Coral)
bocasdata$Soft.or.Fire.Coral <- as.numeric(bocasdata$Soft.or.Fire.Coral)
bocasdata$Zoanthid <- as.numeric(bocasdata$Zoanthid)
bocasdata$Other.Invert <- as.numeric(bocasdata$Other.Invert)
bocasdata$Sponge <- as.numeric(bocasdata$Sponge)
bocasdata$CCA <- as.numeric(bocasdata$CCA)
bocasdata$Macroalgae <- as.numeric(bocasdata$Macroalgae)
bocasdata$Rubble.or.Sand <- as.numeric(bocasdata$Rubble.or.Sand)

#Creating dataframe with average values
bocasavgdata <- bocasdata %>%
  group_by(Site, `Time.Point`) %>%   # Group by Site and Time Point
  summarize(
    LiveHardCoral = mean(`Live.Hard.Coral`, na.rm = TRUE),
    SoftorFireCoral = mean(`Soft.or.Fire.Coral`, na.rm = TRUE),
    DeadCoral = mean(`Dead.Coral`, na.rm = TRUE),
    Zoanthid = mean(`Zoanthid`, na.rm = TRUE),
    Sponge = mean(`Sponge`, na.rm = TRUE),
    OtherInvertebrate = mean(`Other.Invert`, na.rm = TRUE),
    Macroalgae = mean(`Macroalgae`, na.rm = TRUE),
    Substrate = mean(`Rubble.or.Sand`, na.rm = TRUE)
  ) %>%
  ungroup()

#Adding Inner/Outer Bay Classification
bocasdata <- bocasdata %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))
  
#Separating sites
Almirante= bocasavgdata[grep("Almirante", bocasavgdata$Site),]
Caracol= bocasavgdata[grep("Caracol", bocasavgdata$Site),]
CaracolDeep= bocasavgdata[grep("Caracol Deep", bocasavgdata$Site),]
CayoRoldan= bocasavgdata[grep("Cayo Roldan", bocasavgdata$Site),]
CoralCay= bocasavgdata[grep("Coral Cay", bocasavgdata$Site),]
HospitalPoint= bocasavgdata[grep("Hospital Point", bocasavgdata$Site),]
IslandPoint= bocasavgdata[grep("Island Point", bocasavgdata$Site),]
Mainland= bocasavgdata[grep("Mainland", bocasavgdata$Site),]
Pastores= bocasavgdata[grep("Pastores", bocasavgdata$Site),]
Pollito= bocasavgdata[grep("Pollito", bocasavgdata$Site),]
Popa= bocasavgdata[grep("Popa", bocasavgdata$Site),]
PuntaJuan= bocasavgdata[grep("Punta Juan", bocasavgdata$Site),]
SaltCreek= bocasavgdata[grep("Salt Creek", bocasavgdata$Site),]
STRI= bocasavgdata[grep("STRI", bocasavgdata$Site),]



#Trying to run all site NMDS
Sites<-as.vector(bocasavgdata$Site)
NMDSmatrix=as.matrix(bocasavgdata[, -c(1, 2)])
bocasNMDS=metaMDS(NMDSmatrix, k=2, trymax=100)

##Using the scores function from vegan to extract the site scores and convert to a data.frame
bocas.scores <- as.data.frame(vegan::scores(bocasNMDS)$sites)
##Create column of time points
bocas.scores$TimePoint <- bocasavgdata$Time.Point
##Add the site variable
bocas.scores$Site <- Sites 
head(bocas.scores)
##Using the scores function from vegan to extract the species scores and convert to a data.frame
benthic.scores <- as.data.frame(vegan::scores(bocasNMDS, "species"))
# create a column of benthic categories, from the rownames of benthic.scores
benthic.scores$Category <- rownames(benthic.scores) 

#Creating color scheme for sites. 14 sites = 14 colors.
plot.col=c("plum2","violetred1","mediumorchid1","salmon","orangered","gold","palegreen","turquoise","mediumseagreen","slategray1","deepskyblue1","lightslateblue","bisque","dimgray")

NMDS.over.time= ggplot() + 
  theme_classic(base_size=14) +
  ggtitle("Trajectory of Benthic Community Composition in Bocas del Toro") +
  geom_path(data=bocas.scores,aes(x=NMDS1,y=NMDS2,colour=Site), linewidth=1) + # add the point markers
  scale_color_manual(values = plot.col)+ #changing color palette to predefined colors
  geom_text(data=bocas.scores,aes(x=NMDS1,y=NMDS2,label=TimePoint),size=3) + # add the date labels 
  geom_text(data=benthic.scores,aes(x=NMDS1,y=NMDS2,label=Category), size=3) +  # add the species labels
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  annotate(geom="label", x=-0.6, y=-0.5, label="Stress=0.09", size=4.5)

NMDS.over.time

ggsave("BocasNMDS.over.time.jpg", plot = NMDS.over.time, scale = 1, dpi = 300)

#Running MDS, Almirante
Almirante.sites <- as.vector(Almirante$Site)
NMDSAlmirantematrix=as.matrix(Almirante[, -c(1, 2)])
NMDSAlmirante=metaMDS(NMDSAlmirantematrix, k=2, trymax=100)

#Using the scores function from vegan to extract the site scores and convert to a data.frame
Almirante.scores <- as.data.frame(vegan::scores(NMDSAlmirante)$sites)

#Create column of time points
Almirante.scores$TimePoint <- Almirante$Time.Point

#  add the site variable
Almirante.scores$Site <- Almirante.sites 

#look at the data
head(Almirante.scores)

#Using the scores function from vegan to extract the species scores and convert to a data.frame
Alm.benthic.scores <- as.data.frame(vegan::scores(NMDSAlmirante, "species"))

# create a column of benthic categories, from the rownames of benthic.scores
Alm.benthic.scores$Category <- rownames(Alm.benthic.scores) 

head(Alm.benthic.scores)


#PLOT
Almirante_NMDS.over.time= ggplot() + 
  theme_classic(base_size=14) +
  ggtitle("Trajectory of Benthic Community Composition at Almirante") +
  geom_path(data=Almirante.scores,aes(x=NMDS1,y=NMDS2,colour=Site), linewidth=1) + # add the point markers
  geom_text(data=Almirante.scores,aes(x=NMDS1,y=NMDS2,label=TimePoint),size=3) + # add the date labels 
  geom_text(data=Alm.benthic.scores,aes(x=NMDS1,y=NMDS2,label=Category), size=3) +  # add the species labels
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  annotate(geom="label", x=-0.6, y=-0.5, label="Stress=0.09", size=4.5)

Almirante_NMDS.over.time

ggsave("Almirante_NMDS.over.time.jpg", plot = Almirante_NMDS.over.time, scale = 1, dpi = 300)

#Running PERMANOVA

#Selecting benthic community data
community_data <- bocasdata[, -c(1, 2,12)]

permANOVA<-adonis2(community_data ~ Site * Time.Point, data = bocasdata, method = "bray", permutations = 999)

print(permANOVA)

siteposthoc=pairwise.adonis(bocasdata[,3:11], bocasdata$Site)
siteposthoc

timeposthoc=pairwise.adonis(bocasdata[,3:11],bocasdata$Time.Point)
timeposthoc

#Running PERMANOVA on data, with Inner/Outer bay sites compared
bocasdata$BayType<-as.factor(bocasdata$BayType)
innerouterdist_matrix <- vegdist(community_data, method = "bray")
innerouterPERMANOVA <- adonis2(innerouterdist_matrix ~ BayType*Time.Point, data = bocasdata, permutations = 999)
print(innerouterPERMANOVA)
innerouterPERMANOVA
summary(innerouterPERMANOVA)

# Save the result as a text file
capture.output(innerouterPERMANOVA, file = "bocasPERMANOVAresults.txt")

# Check the working directory for "permanova_results.txt"
