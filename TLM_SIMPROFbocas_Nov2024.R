library(vegan)
library(devtools)
install_github("cran/clustsig")
library(clustsig)
library(ggplot2)
library(dplyr)
library(ggdendro) 

####T0

T0<-read.csv("T0.no.islandpoint.csv")

# Create a new data frame with mean percent cover values per site
meanT0 <- T0 %>%
  group_by(Site) %>%                     # Group data by site
  summarise(across(where(is.numeric),    # Calculate mean for numeric columns (functional groups)
                   \(x) mean(x, na.rm = TRUE)))  # Use anonymous function for mean with na.rm
#Adding bay type designations
meanT0 <- meanT0 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

# Make sure 'site' and 'BayType' are factors
meanT0$Site <- factor(meanT0$Site)
meanT0$BayType <- factor(meanT0$BayType)

#Extract site & bay type b/c SIMPROF must be run on numerical data. Also extracting "Substrate" & "Other" columns to limit analysis to living functional groups.

T0benthicdata<-meanT0[,-c(1,10,11)]


# Compute the Bray-Curtis dissimilarity matrix
distanceT0<- vegdist(T0benthicdata, method = "bray")

# Perform hierarchical clustering (UPGMA method) on the dissimilarity matrix
hcT0 <- hclust(distanceT0, method = "average")
hcT0$labels<-meanT0$Site

# Perform SIMPROF analysis (Significance test for hierarchical clustering)
simprofT0<- simprof(distanceT0, num.simulated = 999)  # 999 permutations for significance testing

# Print SIMPROF results
print(simprofT0)

# Convert to dendrogram for plotting
dendT0 <- as.dendrogram(hcT0)

png("T0dendro.png", width = 800, height = 600)

#Creating plot
plot(dendT0, main = "T0 Dendrogram")
rect.hclust(hcT0, k = length(simprofT0$significantclusters), border = "red")

dev.off()


###T3
T3<-read.csv("T3.csv")

# Create a new data frame with mean percent cover values per site
meanT3 <- T3 %>%
  group_by(Site) %>%                     # Group data by site
  summarise(across(where(is.numeric),    # Calculate mean for numeric columns (functional groups)
                   \(x) mean(x, na.rm = TRUE)))  # Use anonymous function for mean with na.rm

#Adding bay type designations
meanT3 <- meanT3 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

# Make sure 'site' and 'BayType' are factors
meanT3$Site <- factor(meanT3$Site)
meanT3$BayType <- factor(meanT3$BayType)

#Extract site & bay type b/c SIMPROF must be run on numerical data. Also extracting "Substrate" & "Other" columns to limit analysis to living functional groups.

T3benthicdata<-meanT3[,-c(1,10,11)]


# Compute the Bray-Curtis dissimilarity matrix
distanceT3<- vegdist(T3benthicdata, method = "bray")

# Perform hierarchical clustering (UPGMA method) on the dissimilarity matrix
hcT3 <- hclust(distanceT3, method = "average")
hcT3$labels<-meanT3$Site

# Perform SIMPROF analysis (Significance test for hierarchical clustering)
simprofT3<- simprof(distanceT3, num.simulated = 999)  # 999 permutations for significance testing

# Print SIMPROF results
print(simprofT3)

# Convert to dendrogram for plotting
dendT3 <- as.dendrogram(hcT3)

png("T3dendro.png", width = 800, height = 600)

#Creating plot
plot(dendT3, main = "T3 Dendrogram")
rect.hclust(hcT3, k = length(simprofT3$significantclusters), border = "red")

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
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

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
plot(dendT9, main = "T9 Dendrogram")
rect.hclust(hcT9, k = length(simprofT9$significantclusters), border = "red")


# Close the device
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
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

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
plot(dendT9, main = "T9 Dendrogram")
rect.hclust(hcT9, k = length(simprofT9$significantclusters), border = "red")


# Close the device
dev.off()


