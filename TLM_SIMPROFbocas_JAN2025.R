library(vegan)
library(devtools)
install_github("cran/clustsig")
library(clustsig)
library(ggplot2)
library(dplyr)
library(ggdendro) 

####T0, T1

T0T1<-read.csv("T0T1.csv")

#Create a new data frame with mean percent cover values per site
meanT0T1 <- T0T1 %>%
  group_by(Site) %>%                     # Group data by site
  summarise(across(where(is.numeric),    # Calculate mean for numeric columns (functional groups)
                   \(x) mean(x, na.rm = TRUE)))  # Use anonymous function for mean with na.rm
#Adding bay type designations
meanT0T1 <- meanT0T1 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol","STRI","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Coral Cay","Popa") ~ "Outer",))

# Make sure 'site' and 'BayType' are factors
meanT0T1$Site <- factor(meanT0T1$Site)
meanT0T1$BayType <- factor(meanT0T1$BayType)

#Extract site & bay type b/c SIMPROF must be run on numerical data. Also extracting "Substrate" column to limit analysis to living functional groups.

T0T1benthicdata<-meanT0T1[,-c(1,10,11)]


# Compute the Bray-Curtis dissimilarity matrix
distanceT0T1<- vegdist(T0T1benthicdata, method = "bray")

# Perform hierarchical clustering (UPGMA method) on the dissimilarity matrix
hcT0T1 <- hclust(distanceT0T1, method = "average")
hcT0T1$labels<-meanT0T1$Site

# Perform SIMPROF analysis (Significance test for hierarchical clustering)
simprofT0T1<- simprof(distanceT0T1, num.simulated = 999)  # 999 permutations for significance testing

# Print SIMPROF results
print(simprofT0T1)

# Convert to dendrogram for plotting
dendT0T1 <- as.dendrogram(hcT0T1)

png("T0T1dendro.png", width = 800, height = 600)

#Creating plot
plotT0T1<-plot(dendT0T1, main = "T0, T1 Dendrogram")
rect.hclust(hcT0T1, k = length(simprofT0T1$significantclusters), border = "red")

dev.off()


####T9

T9<-read.csv("T9.csv")

# Create a new data frame with mean percent cover values per site
meanT9 <- T9 %>%
  group_by(Site) %>%                     # Group data by site
  summarise(across(where(is.numeric),    # Calculate mean for numeric columns (functional groups)
                   \(x) mean(x, na.rm = TRUE)))  # Use anonymous function for mean with na.rm
#Adding bay type designations
meanT9 <- meanT9 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol","STRI","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Coral Cay","Popa") ~ "Outer",))

#Make sure 'site' and 'BayType' are factors
meanT9$Site <- factor(meanT9$Site)
meanT9$BayType <- factor(meanT9$BayType)

#Extract site & bay type b/c SIMPROF must be run on numerical data. Also extracting "Substrate" & "Other" columns to limit analysis to living functional groups.

T9benthicdata<-meanT9[,-c(1,10,11)]

# Compute the Bray-Curtis dissimilarity matrix
distanceT9<- vegdist(T9benthicdata, method = "bray")

# Perform hierarchical clustering (UPGMA method) on the dissimilarity matrix
hcT9 <- hclust(distanceT9, method = "average")
hcT9$labels<-meanT9$Site

# Perform SIMPROF analysis (Significance test for hierarchical clustering)
simprofT9<- simprof(distanceT9, num.simulated = 999)  # 999 permutations for significance testing

# Print SIMPROF results
print(simprofT9)

# Convert to dendrogram for plotting
dendT9 <- as.dendrogram(hcT9)

png("T9dendro.png", width = 800, height = 600)

#Plot w/ significant clusters
plotT9<-plot(dendT9, main = "T9 Dendrogram")
rect.hclust(hcT9, k = length(simprofT9$significantclusters), border = "red")
plotT9

# Close the device
dev.off()


#Code to make this a two-paneled figure
png("T0T1_T9 Dendro.png", width = 800, height = 400)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))

plot(dendT0T1, main = "T0, T1 Dendrogram")
rect.hclust(hcT0T1, k = length(simprofT0T1$significantclusters), border = "red")

plot(dendT9, main = "T9 Dendrogram")
rect.hclust(hcT9, k = length(simprofT9$significantclusters), border = "red")

dev.off()
